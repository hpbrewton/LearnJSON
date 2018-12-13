module BooleanAlgebra (
	BooleanAlgebra,
	Capture(..),
	ConvexInterval(..), 
	seperate,
	meet,
	join,
	not,
	bot,
	top,
	simplify,
	captureAccept,
	convexFromSet,
	pairwiseDisjoint,
	captureSingleton
	) where 

import qualified Data.Set as S
import Prelude hiding (not)
import qualified Prelude as P (not)

class BooleanAlgebra p where 
	seperate :: [p] -> [p]
	meet :: p  -> p  -> p  
	join :: p  -> p  -> p  
	not :: p  -> p  
	bot :: p  
	top :: p 
	simplify :: p -> p 

pairwiseDisjoint :: (BooleanAlgebra p, Eq p) => [p] -> Bool 
pairwiseDisjoint [] = True 
pairwiseDisjoint (x:xs) = (and $ map (\y -> bot /= (simplify $ (join x y))) xs) && pairwiseDisjoint xs 

data Capture a = All 
	| None 
	| Some (S.Set a)
	| NotSome (S.Set a)
	deriving (Show, Eq, Ord)

captureSimplify :: (Capture a) -> (Capture a)
captureSimplify (Some s)
	| (0 == S.size s) = None 
	| otherwise = (Some s)
captureSimplify (NotSome s)
	| (0 == S.size s) = All 
	| otherwise = (NotSome s)
captureSimplify x = x 

captureMember :: (Ord a) => (Capture a) -> a -> Bool 
captureMember All a = True 
captureMember None a = True 
captureMember (Some s) a = S.member a s 
captureMember (NotSome s) a = S.notMember a s 

captureMeet :: (Ord a) => (Capture a) -> (Capture a) -> (Capture a)
captureMeet None _ = None 
captureMeet All v = v 
captureMeet (Some a) (Some b) = Some $ S.intersection a b 
captureMeet (NotSome a) (NotSome b) = NotSome $ S.union a b 
captureMeet (Some a) (NotSome b) = Some $ a S.\\ b 
captureMeet a b = captureMeet b a 

captureJoin :: (Ord a) => (Capture a) -> (Capture a) -> (Capture a)
captureJoin None v = v 
captureJoin All _ = All 
captureJoin (Some a) (Some b) = Some $ S.union a b 
captureJoin (NotSome a) (NotSome b) = NotSome $ S.intersection a b 
captureJoin (Some a) (NotSome b) = NotSome $ b S.\\ a 
captureJoin a b = captureJoin b a 

captureNot :: (Capture a) -> (Capture a)
captureNot All = None 
captureNot None = All 
captureNot (Some a) = NotSome a 
captureNot (NotSome a) = Some a 

captureTop = All 
captureBot = None 

-- | TODO: here need to find the maximizer (not just first)
captureSeperate :: (Ord a) => [Capture a] -> [Capture a]
captureSeperate cs 
	| (((length cs ) - countNone) == 1) = map (\x -> if x == None then None else All) cs
	| otherwise = left ++ ((meet m (foldr (join . not) bot (left ++ right))) : right )
	where 
		countNone = length $ filter (== None) cs
		size None = 0
		size All = 0 
		size (Some a) = S.size a 
		size (NotSome a) = S.size a
		maximizer = maximum $ map size cs
		(left, m:right) = break ((==maximizer) . size) cs 

captureSingleton :: a -> Capture a 
captureSingleton = Some . S.singleton 

captureAccept :: (Ord a) => Capture a -> a -> Bool 
captureAccept None _ = False 
captureAccept All  _ = True 
captureAccept (Some s) i = S.member i s 
captureAccept (NotSome s) i = not (S.member i s) 

instance (Ord a) => BooleanAlgebra (Capture a) where
	seperate = captureSeperate
	meet x = captureSimplify . (captureMeet x)
	join x = captureSimplify . (captureJoin x)
	not = captureSimplify . captureNot 
	top = captureSimplify $ captureTop
	bot = captureSimplify $ captureBot
	simplify = captureSimplify


-- | The convex interval boolean algebra is a little longing (what is the negation of a convex interval)
data ConvexInterval a = ConvexInterval a a 
	deriving (Eq, Ord, Show)

convexBot :: (Num a) => ConvexInterval a 
convexBot = ConvexInterval 0 0 

convexTop :: (Bounded a) => ConvexInterval a 
convexTop = ConvexInterval minBound maxBound

convexMember :: (Ord a) => ConvexInterval a -> a -> Bool 
convexMember (ConvexInterval l u) v = (v >= l) && (v < u)

convexMeet :: (Ord a, Num a) => ConvexInterval a -> ConvexInterval a -> ConvexInterval a 
convexMeet (ConvexInterval l1 r1) (ConvexInterval l2 r2) 
	| r < l = convexBot
	| otherwise = ConvexInterval l r 
	where 
		r = (max r1 r2)
		l = (min l1 l2)

convexJoin :: (Ord a, Num a) => ConvexInterval a -> ConvexInterval a -> ConvexInterval a 
convexJoin (ConvexInterval l1 r1) (ConvexInterval l2 r2) = ConvexInterval (min l1 l2) (max r1 r2)

convexNot :: (Bounded a, Eq a, Num a, Ord a) => ConvexInterval a -> ConvexInterval a 
convexNot (ConvexInterval l r)
	| l == r = top 
	| otherwise =  ConvexInterval r l 

convexSeperate :: (Num a, Ord a, Eq a, Bounded a) => [ConvexInterval a] -> [ConvexInterval a]
convexSeperate (c:cs) 
	| (countNotBot == 1) = map (\x -> if x == bot then bot else top) (c:cs)
	| otherwise = ((foldr (join . not) bot cs) : cs )
	where 
		notSome = foldr (join . not) bot (cs)
		countNotBot = length $ filter (/= bot) (c:cs)

convexFromSet :: (Num a, Ord a, Eq a, Bounded a) => S.Set a -> ConvexInterval a 
convexFromSet s
	| (S.size s) == 0 = bot
	| otherwise = ConvexInterval (minimum s) (maximum s)

convexSimplify :: (Eq a, Ord a, Num a, Bounded a) => ConvexInterval a -> ConvexInterval a 
convexSimplify (ConvexInterval l r)
	| l == r = bot 
	| otherwise = ConvexInterval l r 

instance (Bounded a, Num a, Ord a, Eq a) => BooleanAlgebra (ConvexInterval a) where
	seperate = convexSeperate
	meet = convexMeet
	join = convexJoin 
	not = convexNot 
	top = convexTop
	bot = convexBot
	simplify = convexSimplify

instance BooleanAlgebra Bool where
	seperate = id 
	meet = (&&)
	join = (||)
	not = (P.not)
	top = True 
	bot = False 
	simplify = id



	
