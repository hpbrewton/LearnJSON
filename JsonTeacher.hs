{-# LANGUAGE MultiParamTypeClasses #-}

module JsonTeacher (
	JsonTeacher(..)
	)
	where 

import qualified Tree as T
import qualified Teacher as O 
import qualified JsonAlgebra as JA
import qualified Text.JSON as JSON
import qualified TreeAutomata as Aut
import qualified Data.Set as S 
import qualified Data.Map as M 
import qualified Data.List as List
import qualified Numeric as Numeric
import qualified JsonSchema as JSS 

data JsonTeacher = JsonTeacher

getValue :: T.Tree JA.JsonLetter -> Maybe String 
getValue (T.Tree (JA.Number f) []) = return $ show f 
getValue (T.Tree (JA.Str s) []) = return $ show s 
getValue (T.Tree (JA.Boolean b) []) = return $ show b 
getValue (T.Tree (JA.Control JA.Null) []) = return $ "null"
getValue (T.Tree (JA.Control JA.Array) [child]) = do 
	aps <- getAps child
	return $ '[' : (aps ++ "]")
	where 
		getAps :: T.Tree JA.JsonLetter -> Maybe String 
		getAps (T.Tree (JA.Control JA.End) []) = return ""
		getAps (T.Tree (JA.KVP key) [value, cons]) = do 
			valueStr <- getValue value
			rest <- getAps cons
			return $ valueStr ++ ',':rest
		getAps _ = Nothing
getValue (T.Tree (JA.Control JA.Object) [child]) = do 
	kvps <- getKvps child
	return $ '{' : (kvps ++ "}")
	where 
		getKvps :: T.Tree JA.JsonLetter -> Maybe String 
		getKvps (T.Tree (JA.KVP key) [value, T.Tree (JA.Control JA.End) []]) = do 
			valueStr <- getValue value
			return $ (show key) ++ (':':valueStr) 
		getKvps (T.Tree (JA.KVP key) [value, cons]) = do 
			valueStr <- getValue  value
			rest <- getKvps cons
			return $ (show key) ++ (':':valueStr) ++ ',':rest
		getKvps _ = Nothing
getValue _ = Nothing

noDuplicateKeys :: T.Tree JA.JsonLetter -> Bool 
noDuplicateKeys (T.Tree (JA.Control JA.Object) [kvp]) = ((length strs) == (S.size $ S.fromList strs)) && (and $ map noDuplicateKeys lower)
	where 
		zoomKeys :: T.Tree JA.JsonLetter -> [(String, T.Tree JA.JsonLetter)]
		zoomKeys (T.Tree (JA.KVP key) [value, T.Tree (JA.Control JA.End) []]) = [(key, value)]
		zoomKeys (T.Tree (JA.KVP key) [value, cons]) = (key, value) : (zoomKeys cons)
		(strs, lower) = unzip $ zoomKeys kvp 
noDuplicateKeys (T.Tree _ children) = and $ map noDuplicateKeys children

keysNotOutOfOrder :: T.Tree JA.JsonLetter -> Bool 
keysNotOutOfOrder (T.Tree (JA.Control JA.Object) [kvp]) = (increasing strs) && (and $ map keysNotOutOfOrder lower)
	where 
		zoomKeys :: T.Tree JA.JsonLetter -> [(String, T.Tree JA.JsonLetter)]
		zoomKeys (T.Tree (JA.KVP key) [value, T.Tree (JA.Control JA.End) []]) = [(key, value)]
		zoomKeys (T.Tree (JA.KVP key) [value, cons]) = (key, value) : (zoomKeys cons)
		(strs, lower) = unzip $ zoomKeys kvp 
		increasing :: (Ord a) => [a] -> Bool 
		increasing [] = True 
		increasing [x] = True 
		increasing (x:y:ys) = (x <= y) && (increasing (y:ys))
keysNotOutOfOrder (T.Tree _ children) = and $ map keysNotOutOfOrder children

unload :: T.Tree JA.JsonLetter -> Maybe String 
unload tree@(T.Tree (JA.Control JA.Object) [kvp]) = getValue tree 
unload _ = Nothing

loadAux :: JSON.JSValue -> T.Tree JA.JsonLetter
loadAux JSON.JSNull = (T.Tree (JA.Control JA.Null) [])
loadAux (JSON.JSBool b) = (T.Tree (JA.Boolean b) [])
loadAux (JSON.JSString s) = (T.Tree (JA.Str $ JSON.fromJSString s) [])
loadAux (JSON.JSArray elems) = (T.Tree (JA.Control JA.Array) $ return $ unfoldElems elems)
	where
		unfoldElems :: [JSON.JSValue] -> T.Tree JA.JsonLetter
		unfoldElems = foldr (\x cons -> T.Tree (JA.Control JA.AP) [x, cons]) (T.Tree (JA.Control JA.End) []) . map loadAux . reverse 
loadAux (JSON.JSObject object) = (T.Tree (JA.Control JA.Object) $ return $ unfoldElems $ JSON.fromJSObject object)
	where
		unfoldElems :: [(String, JSON.JSValue)] -> T.Tree JA.JsonLetter 
		unfoldElems = foldr (\(k, v) cons -> T.Tree (JA.KVP k) [loadAux v, cons]) (T.Tree (JA.Control JA.End) []) . List.sortBy (\ a b -> compare (fst a) (fst b))
loadAux (JSON.JSRational _ rat) = (T.Tree (JA.Number  ((Numeric.fromRat rat) :: Float)) [])

load :: String -> T.Tree JA.JsonLetter
load str = (case (JSON.decode :: String -> JSON.Result JSON.JSValue)  str of 
	(JSON.Ok a) -> loadAux a
	(JSON.Error str) -> error str 
	)

jsonMember :: JsonTeacher -> T.Tree JA.JsonLetter -> IO Bool 
jsonMember teacher tree = case unload tree of 
	Just str -> do 
		if ((noDuplicateKeys tree) && (keysNotOutOfOrder tree))
			then (do 
				putStrLn $ str
				putStrLn "memb?"
				response <- getLine
				return (response == "y")
			)
			else return False
	Nothing -> return False 

jsonEquivalent :: (S.Set JA.JsonLetter -> JA.JsonPredicate) -> JsonTeacher -> Aut.TreeAutomata JA.JsonLetter -> IO (Maybe (T.Tree JA.JsonLetter))
jsonEquivalent general teach aut = do 
	putStrLn "eq?"
	-- putStrLn $ show aut
	case Aut.generalize general aut of 
		Just saut -> do 
			-- putStrLn $ show $ saut
			putStrLn $ show $ JSS.jsonSchema saut 
		Nothing -> putStrLn "" 
	answer <- getLine 
	if (answer == "y")
		then return $ Nothing 
		else return $ Just $ load answer

instance O.Teacher JsonTeacher JA.JsonLetter where 
	member = jsonMember
	equivalent = (jsonEquivalent JA.jsonAlgebraFromSet)