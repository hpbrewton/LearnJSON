module Main (
	main 
	) where 

import qualified LearnJSON.Tree as T
import qualified LearnJSON.Teacher as O
import qualified Data.Map as M 
import qualified Data.Set as S 

data ObsTable a = ObsTable {
	f :: M.Map (T.Tree a) Bool,
	e :: S.Set (T.Context a),
	s :: S.Set (T.Tree a),
	r :: S.Set (T.Tree a)
	}
	deriving (Eq)

instance (Ord a, Show a) => Show (ObsTable a) where
	show table = unlines $ (show elist) : "S: " : [showRow tree | tree <- slist] ++ "R: " : [showRow tree | tree <- rlist]
		where
			elist = S.toList $ e table 
			slist = S.toList $ s table 
			rlist = S.toList $ r table 
			maxSize = foldr (max) 0 $ map (length . show) (slist ++ rlist)
			pad n str = str ++ replicate (max 0 (n - length str)) ' '
			showRow tree = (pad maxSize $ show tree) ++ ": " ++  concat [ if (T.mkTree cntx tree) `M.member` (f table) 
					then  (if ((f table) M.! (T.mkTree cntx tree)) then "+ " else "- ")
					else  ("_ ")
				| cntx <- elist]


mkTable :: (Ord a) => a -> ObsTable a
mkTable a = ObsTable f e s r 
	where
		f = M.empty 
		e = S.singleton $ T.Context [] T.EmptyTree
		s = S.singleton T.EmptyTree
		r = S.singleton $ T.Tree a []

fill :: (Ord a, Show a, O.Teacher t) => t a -> ObsTable a -> IO (ObsTable a)
fill teacher table = do 
	let pairs = [T.mkTree cntxt tree | cntxt <- S.toList $ e table, tree <- S.toList $ (s table) `S.union` (r table)]
	let unseen = filter (\t -> M.notMember t (f table)) pairs
	results <- sequence $ [O.member teacher tree | tree <- unseen]
	let newSet = M.fromList $ zip unseen results
	return table{f = M.union (f table) newSet}

row :: (Ord a) => ObsTable a -> [T.Context a] -> T.Tree a -> [Bool]
row table elist tree = [(f table) M.! (T.mkTree cntxt tree)| cntxt <- elist]

close :: (Ord a) => T.Context a -> ObsTable a -> ObsTable a 
close arb table = table{s = (s table) `S.union` rsToPromote, r = ((r table) `S.union` nsToAdd) `S.difference` rsToPromote}
	where
		elist = S.toList $ e table 
		sRowSet = S.map (row table elist) $ s table
		rsToPromote = S.filter (\r -> (row table elist r) `S.notMember` sRowSet) (r table)
		nsToAdd = S.map (T.mkTree arb) rsToPromote 


evidenceClose :: (Ord a) => ObsTable a -> ObsTable a 
evidenceClose table = table{r = ((r table) `S.union` nonMembers) `S.union` unseenPreficies}
	where 
		sur = S.union (s table) (r table)
		sByE = S.foldr (S.union) S.empty $ S.map (\tree -> S.map (\cntxt -> T.mkTree cntxt tree) (e table)) (s table)
		nonMembers = S.filter (\t -> t `S.notMember` sur) $ sByE
		unseenPreficies = S.filter (\t -> t `S.notMember` sur) $ S.foldr S.union S.empty $ S.map (S.fromList . T.preficies) nonMembers
 
-- makeConsistent :: (Ord a) => ObsTable a -> ObsTable a 
makeConsistent table = table {e = (e table) `S.union` leadDifferently}
	where
		sur = S.union (s table) (r table)
		elist = S.toList (e table)
		sameRows = S.filter (\(a, b) -> (row table elist a) == (row table elist b)) $ S.cartesianProduct sur sur 
		sufficiesOf b = S.foldr (S.union) S.empty $ S.map (\a -> T.sufficies a b) $ sur 
		commonSuffices = S.filter (\(a,b,c) -> S.empty /= c) $ S.map (\(a, b) -> (a, b, (sufficiesOf a) `S.intersection` (sufficiesOf b))) sameRows
		commonSuffices' = S.foldr (S.union) S.empty $ S.map (\(a,b,c) -> S.map (\ cntxt -> (a, b, cntxt)) c) commonSuffices
		newSufficies = S.filter (\(a,b,c) -> S.notMember c (e table)) commonSuffices'
		leadDifferently = S.map (\(_, _, c) -> c) $ S.filter (\(a, b, c) -> (row table elist (T.mkTree c a)) /= (row table elist (T.mkTree c b))) newSufficies
		-- TODO not quite done

cohesive :: (Ord a, Show a, O.Teacher t) => t a -> a -> ObsTable a -> IO (ObsTable a)
cohesive teacher arb table = do 
	ft <- fill teacher table 
	let ct = close (T.Context [0] (T.Tree arb [T.EmptyTree])) ft
	fct <- fill teacher ct 
	let ect = evidenceClose fct 
	fect <- fill teacher ect 
	let mect = makeConsistent fect
	fmect <- fill teacher mect 
	if fmect == table 
		then return table 
		else cohesive teacher arb fmect

lstarLoop :: (Ord a, Read a, Show a, O.Teacher t) => t a -> a -> ObsTable a -> IO (ObsTable a)
lstarLoop teacher arb table = do 
	ctable <- cohesive teacher arb table 
	putStrLn $ show ctable 
	maybeCounterExample <- O.equivalent teacher
	case maybeCounterExample of 
		Nothing -> return $ ctable
		Just counterExample -> do 
			let sur = S.union (s table) (r table)
			let prefs = (T.preficiesSet counterExample)
			let newPrefs = (prefs S.\\ sur)
			let newTable = ctable {r = (r ctable) `S.union` newPrefs}
			lstarLoop teacher arb newTable


main :: IO ()
main = do 
	table <- lstarLoop O.IOTeacher 4 $ mkTable 52
	putStrLn $ show table
	putStrLn $ "done."

-- main = do 
-- 	let s = T.Tree 5 [T.Tree 4 []] 
-- 	let t = T.Tree 3 [T.Tree 2 [s], T.Tree 1 [T.Tree 0 [s]]]
-- 	putStrLn $ show $ T.sufficies t t

