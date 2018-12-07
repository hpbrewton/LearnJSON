module LearnJSON.Tree (
	Tree(..),
	Context(..),
	mkTree, 
	preficies,
	preficiesSet,
	indicies,
	sufficies,
	clearCntxt
	) where

import qualified Data.Set as S

data Tree a = Tree a [Tree a]
	| EmptyTree
	deriving (Show, Ord, Eq, Read)

data Context a = Context [Int] (Tree a)
	| NoContext
	deriving (Show, Ord, Eq)

mkTree :: Context a -> Tree a -> Tree a 
mkTree (NoContext) tree = tree 
mkTree (Context [] _) tree = tree 
mkTree (Context (x:xs) (Tree v cs)) tree  = Tree v ((left) ++ middle : (right))
	where
		left = take x cs 
		(m:right) = drop x cs 
		middle = mkTree (Context xs m) tree 

preficies :: Tree a -> [Tree a]
preficies EmptyTree = []
preficies tree@(Tree _ cs) = tree : concatMap preficies cs 

preficiesSet :: (Ord a) => Tree a -> S.Set (Tree a)
preficiesSet EmptyTree = S.empty
preficiesSet tree@(Tree _ cs) = S.insert tree $ S.unions $ map preficiesSet cs 

indicies :: Tree a -> [(Tree a, [Int])]
indicies tree = map (\(t, path) -> (t, reverse path)) $ indiciesAux tree $ [] 
	where 
		indiciesAux :: Tree a -> [Int] -> [(Tree a, [Int])]
		indiciesAux EmptyTree stack = [(EmptyTree, stack)]
		indiciesAux tree@(Tree _ cs) stack = (tree, stack) : (concatMap (\(n, child) -> indiciesAux child (n:stack)) $ zip [0..] cs)

clearCntxt :: Context a -> Context a 
clearCntxt NoContext = NoContext
clearCntxt (Context [] _) = Context [] EmptyTree
clearCntxt (Context (x:xs) (Tree v cs)) = Context (x:xs) (Tree v ((left) ++ tree : (right)))
	where
		left = take x cs 
		(m:right) = drop x cs 
		(Context _ tree) = clearCntxt (Context xs m)

sufficies :: (Ord a) => Tree a -> Tree a -> S.Set (Context a)
sufficies a b = S.map clearCntxt $ S.map (\(_, index) -> Context index a) $ S.filter (\(tree, index) -> b == tree) $ S.fromList $ indicies a 