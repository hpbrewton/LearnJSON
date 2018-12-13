module JsonAlgebra (
	JsonLetter(..),
	JsonPredicate(..),
	ControlChar(..),
	accept,
	jsonAlgebraFromSet,
	kvpCapture,
	strCapture,
	controlCapture,
	boolCapture,
	numConvexInterval
	)
	where 

import qualified Data.Set as S 
import qualified Data.List as List
import qualified BooleanAlgebra as BA 

data ControlChar = Null | Array | Object | AP | End 
	deriving (Ord, Read, Show, Eq)

instance Bounded Float where
	minBound = -100.0 
	maxBound = 100.0  

data JsonLetter = Control ControlChar
	| KVP String 
	| Number Float 
	| Str String 
	| Boolean Bool 
	deriving (Show, Ord, Eq, Read)

data JsonPredicate = JsonPredicate {
	kvpCapture :: BA.Capture String,
	strCapture :: BA.Capture String,
	controlCapture :: BA.Capture ControlChar,
	boolCapture :: BA.Capture Bool,
	numConvexInterval :: BA.Capture Float
	}
	deriving (Show, Ord, Eq)

instance BA.BooleanAlgebra JsonPredicate where
	seperate as = map (\(k, s, c, b, i) -> JsonPredicate k s c b i) $ List.zip5 cKVP cSTR cCONT cBOOL iNUM
		where 
			cKVP = BA.seperate (map kvpCapture as)
			cSTR = BA.seperate (map strCapture as)
			cCONT = BA.seperate (map controlCapture as)
			cBOOL = BA.seperate (map boolCapture as)
			iNUM = BA.seperate (map numConvexInterval as)
	meet a b = JsonPredicate cKVP cSTR cCONT cBOOL iNUM
		where 
			cKVP = BA.meet (kvpCapture a) (kvpCapture b)
			cSTR = BA.meet (strCapture a) (strCapture b)
			cCONT = BA.meet (controlCapture a) (controlCapture b)
			cBOOL = BA.meet (boolCapture a) (boolCapture b)
			iNUM = BA.meet (numConvexInterval a) (numConvexInterval b)
	join a b = JsonPredicate cKVP cSTR cCONT cBOOL iNUM
		where 
			cKVP = BA.join (kvpCapture a) (kvpCapture b)
			cSTR = BA.join (strCapture a) (strCapture b)
			cCONT = BA.join (controlCapture a) (controlCapture b)
			cBOOL = BA.join (boolCapture a) (boolCapture b)
			iNUM = BA.join (numConvexInterval a) (numConvexInterval b)
	not a = JsonPredicate cKVP cSTR cCONT cBOOL iNUM
		where 
			cKVP = BA.not (kvpCapture a)
			cSTR = BA.not (strCapture a)
			cCONT = BA.not (controlCapture a)
			cBOOL = BA.not (boolCapture a)
			iNUM = BA.not (numConvexInterval a)
	bot = JsonPredicate BA.bot BA.bot BA.bot BA.bot BA.bot 
	top = JsonPredicate BA.top BA.top BA.top BA.top BA.top
	simplify a = JsonPredicate cKVP cSTR cCONT cBOOL iNUM
		where 
			cKVP = BA.simplify (kvpCapture a)
			cSTR = BA.simplify (strCapture a)
			cCONT = BA.simplify (controlCapture a)
			cBOOL = BA.simplify (boolCapture a)
			iNUM = BA.simplify (numConvexInterval a)

accept :: JsonPredicate -> JsonLetter -> Bool 
accept pred (Control cc) = BA.captureAccept (controlCapture pred) cc 
accept pred (KVP str) = BA.captureAccept (kvpCapture pred) str 
accept pred (Number f) = BA.captureAccept (numConvexInterval pred) f 
accept pred (Str str) = BA.captureAccept (strCapture pred) str 
accept pred (Boolean b) = BA.captureAccept (boolCapture pred) b 

jsonAlgebraFromSingleton :: JsonLetter -> JsonPredicate
jsonAlgebraFromSingleton (Control c) = BA.bot{controlCapture = BA.captureSingleton c}
jsonAlgebraFromSingleton (KVP s) = BA.bot{kvpCapture = BA.captureSingleton s}
jsonAlgebraFromSingleton (Str s) = BA.bot{strCapture = BA.captureSingleton s}
jsonAlgebraFromSingleton (Boolean b) = BA.bot{boolCapture = BA.captureSingleton b}
jsonAlgebraFromSingleton (Number f) = BA.bot{numConvexInterval = BA.captureSingleton f} -- | A single number does not a range make -- | see below where numerical ranges are constructed differently

jsonAlgebraFromSet :: S.Set JsonLetter -> JsonPredicate
jsonAlgebraFromSet set = (jsonAlgebraFromSetAux $ S.toList set) -- {numConvexInterval = numberInterval}
	where 
		-- numberInterval = BA.convexFromSet $ S.map (\(Number f) -> f) $ S.filter (\x -> case x of 
		-- 	(Number f) -> True
		-- 	_ -> False) set 
		jsonAlgebraFromSetAux :: [JsonLetter] -> JsonPredicate
		jsonAlgebraFromSetAux [] = BA.bot 
		jsonAlgebraFromSetAux (x:xs) = BA.join (jsonAlgebraFromSingleton x) (jsonAlgebraFromSetAux xs) 
