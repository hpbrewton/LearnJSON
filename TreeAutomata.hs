module TreeAutomata (
	State,
	TreeAutomata(..),
	generalize
	) where 

import qualified Data.Set as S 
import qualified Data.Map as M
import qualified BooleanAlgebra as BA 
import qualified Data.Maybe as Maybe

type State = Int 

data TreeAutomata a = TreeAutomata {
	numStates :: Int,
	initialState :: Int,
	finalStates ::  S.Set Int,
	transitions :: M.Map ([Int], a) Int 
	}

generalize :: (BA.BooleanAlgebra p, Ord p, Ord a) => (S.Set a -> p) -> TreeAutomata a -> Maybe (TreeAutomata p)
generalize general aut = do 
	let numberOfNothings = M.size $ M.filter (==Nothing) seperatedBundleMap
	if numberOfNothings /= 0
		then Nothing
		else Just $ aut{transitions =  M.fromList [((b, g), t) | (b, gtps) <- M.toList $ M.map (Maybe.fromJust) seperatedBundleMap, (g, t) <- gtps]}
	where
		flipped = map (\((bundle, guard), to) -> ((bundle, to), guard)) $ M.toList $ transitions aut 
		grouped = foldr (\(bt,g) m -> if (M.member bt m)
			then M.adjust (\(gs) -> (S.insert g gs)) bt m 
			else M.insert bt (S.singleton g) m
			) M.empty $ flipped
		bundleMap = foldr (\((b,t),s) m -> if (M.member b m)
			then M.adjust ((t, general s):) b m 
			else M.insert b [(t, general s)] m 
			) M.empty $ M.toList grouped
		seperateToWithGuard edges = Just $ zip (BA.seperate guards) tos 
			-- | BA.pairwiseDisjoint guards = Just $ zip (BA.seperate guards) tos 
			-- | otherwise = Nothing 
			where 
				(tos, guards) = unzip edges
		seperatedBundleMap = M.map seperateToWithGuard bundleMap

instance (Show a) => Show (TreeAutomata a) where
	show aut = "(Q,q0,F,Δ)\nwhere:\n" ++ (unlines $ map (\(a, b) -> '\t':(a ++ ": " ++ b)) [("Q", q), ("q0", q0), ("F", f), ("Δ", d)])
		where 
			q = "1 to " ++ show (numStates aut)
			q0 = show (initialState aut) 
			f = show (finalStates aut)
			d = '\n' : (unlines $ map (('\t':) . show) $ M.toList $ transitions aut)


