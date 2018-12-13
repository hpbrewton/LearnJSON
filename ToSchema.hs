module LearnJSON.ToSchema (
	toSchema 
	)
	where 

import qualified LearnJSON.TreeAutomata as Aut 
import qualified LearnJSON.JsonAlgebra as JA
import qualified Data.Map as M 
import qualified Data.Set as S 

invert :: (Ord a, Ord b) => M.Map a b -> M.Map b (S.Set a)
invert = foldr (\(a, b) m -> if b `M.member` m then M.adjust (S.insert a) b m else M.insert b (S.singleton a) m) M.empty . M.toList 

closure :: (Ord a) => (a -> S.Set a) -> S.Set a -> S.Set a 
closure op = closureAux op S.empty 
	where
		closureAux :: (Ord a) => (a -> S.Set a) -> S.Set a -> S.Set a -> S.Set a 
		closureAux op closed frontier 
			| (S.size frontier) == 0 = closed
			| otherwise = let {Just (f, rest) = (S.maxView frontier)} in closureAux op (S.insert f closed) ((op f  S.\\ closed) `S.union` rest)

-- toSchema :: Aut.TreeAutomata JA.JsonPredicate -> M.Map Int (S.Set ([Int], JA.JsonPredicate))
toSchema aut = kvpClosure
	where 
		m = invert $ Aut.transitions aut
		growKVP n = S.map (\(_:k:_, _) -> k) $ S.filter ((/= JA.None) . JA.kvpPred . snd) $ (m M.! n)
		kvpClosure = closure growKVP $ Aut.finalStates aut

