module JsonSchema (
	jsonSchema
	) where 

import qualified BooleanAlgebra as BA 
import qualified JsonAlgebra as JA 
import qualified Data.Map as M 
import qualified Data.Set as S 
import qualified Text.JSON as JSON
import qualified TreeAutomata as Aut 
import qualified Data.Maybe as Maybe

closure :: (Ord a) => (a -> S.Set a) -> S.Set a -> S.Set a 
closure op = closureAux S.empty op 
	where 
		closureAux seen op frontier 
			| (S.size frontier) == 0 = seen 
			| otherwise              = closureAux (S.union frontier seen) op new 
			where 
				new = (S.foldr S.union S.empty $ S.map op frontier) S.\\ (seen)

invertMap ::  Aut.TreeAutomata JA.JsonPredicate -> M.Map Int (S.Set ([Int], JA.JsonPredicate))
invertMap aut = foldr (\(a, b) m -> if b `M.member` m then M.adjust (S.insert a) b m else (M.insert b (S.singleton a) m)) M.empty $ M.toList (Aut.transitions aut)

valueParticles :: Aut.TreeAutomata JA.JsonPredicate -> M.Map Int JA.JsonPredicate
valueParticles aut = M.fromList $ map (\((_,g),t) -> (t,g)) $ filter (\((b,g),t) -> (0 == length b)) $ M.toList $ Aut.transitions aut 

singleton :: (String, JSON.JSValue) -> JSON.JSValue
singleton = JSON.makeObj . return

jsStr :: String -> JSON.JSValue 
jsStr = JSON.JSString . JSON.toJSString

jsNum :: Float -> JSON.JSValue
jsNum = JSON.JSRational True . toRational 

stringType :: BA.Capture String -> Maybe JSON.JSValue 
stringType BA.None = Nothing
stringType (BA.Some s) = Just $ singleton ("oneOf", JSON.JSArray $ map (jsStr) $ S.toList s)
stringType (BA.NotSome s) = Just $ singleton ("not", singleton ("oneOf", JSON.JSArray $ map (jsStr) $ S.toList s))
stringType BA.All = Just $ singleton (("type"), jsStr "string")

numberType :: BA.Capture Float -> Maybe JSON.JSValue
numberType BA.None = Nothing
numberType (BA.Some s) = Just $ singleton ("oneOf", JSON.JSArray $ map (jsNum) $ S.toList s)
numberType (BA.NotSome s) = Just $ singleton ("not", singleton ("oneOf", JSON.JSArray $ map (jsNum) $ S.toList s))
numberType BA.All = Just $ singleton (("type"), jsStr "number")

boolType :: BA.Capture Bool -> Maybe JSON.JSValue
boolType BA.None = Nothing
boolType (BA.Some s) = Just $ singleton ("oneOf", JSON.JSArray $ map (JSON.JSBool) $ S.toList s)
boolType (BA.NotSome s) = Just $ singleton ("not", singleton ("oneOf", JSON.JSArray $ map (JSON.JSBool) $ S.toList s))
boolType BA.All = Just $ singleton (("type"), jsStr "bool")

nullType :: BA.Capture JA.ControlChar -> Maybe JSON.JSValue
nullType cap = if BA.captureAccept cap (JA.Null) then Just JSON.JSNull else Nothing

particlePredicateToJSONType :: JA.JsonPredicate -> JSON.JSValue
particlePredicateToJSONType pred = singleton ("oneOf", JSON.JSArray $ map Maybe.fromJust $ filter (/=Nothing) [predStringType, predNumberType, predBoolType, predNullType])
	where 
		predStringType = stringType $ JA.strCapture pred 
		predNumberType = numberType $ JA.numConvexInterval pred
		predBoolType = boolType $ JA.boolCapture pred
		predNullType = nullType $ JA.controlCapture pred

-- jsonSchema :: Aut.TreeAutomata JA.JsonPredicate -> Maybe Schema 
jsonSchema aut = M.map (JSON.encode . particlePredicateToJSONType) $ valueParticles aut 
	where 
		iaut = invertMap aut 
		stateClosure = closure (S.map (\([_, c], _) -> c) . S.filter (\(b, _) -> (2 == length b)) . (iaut M.!)) 
		predicates = S.foldr BA.join BA.bot . S.map snd . S.filter (\(b, _) -> (2 == length b)) . S.foldr S.union S.empty . S.map (iaut M.!) 
		oneOf = S.map (predicates . stateClosure . S.singleton) $ S.map (\([kvp], p) -> kvp) $ S.filter (\(b, p) -> (1 == length b) && (JA.accept p (JA.Control JA.Object))) $ S.foldr S.union S.empty $ S.map (iaut M.!) $ Aut.finalStates aut


