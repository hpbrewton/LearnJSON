{-# LANGUAGE MultiParamTypeClasses #-}

module Main where 

import qualified Data.Set as S 

import qualified Teacher as O
import qualified Tree as T 
import qualified ObsTable as OT
import qualified TreeAutomata as Aut
import qualified BooleanAlgebra as BA
import qualified JsonAlgebra as JA
import qualified JsonTeacher as JO

data L = A Int
	deriving (Show, Read, Ord, Eq)

data T = T 

ioMember :: (Show a) => T -> (T.Tree a) -> IO Bool 
ioMember teacher tree = do 
	putStrLn $ show tree 
	putStrLn "memb?"
	answer <- getLine 
	return $ (answer == "y")

ioEquivalent :: (Show a, Read a, Ord a, BA.BooleanAlgebra p, Ord p, Show p) => (S.Set a -> p) -> T -> Aut.TreeAutomata a -> IO (Maybe (T.Tree a))
ioEquivalent general teach aut = do 
	putStrLn "eq?"
	putStrLn $ show aut
	putStrLn $ show $ Aut.generalize general aut 
	answer <- getLine 
	if (answer == "y")
		then return $ Nothing 
		else return $ Just $ read answer

instance O.Teacher T L where
 	member = ioMember 
 	equivalent = (ioEquivalent BA.Some)

instance O.Teacher T Int where
 	member = ioMember
 	equivalent = (ioEquivalent BA.convexFromSet)

instance O.Teacher T JA.JsonLetter where
	member = ioMember
	equivalent = (ioEquivalent JA.jsonAlgebraFromSet)

main :: IO ()
main = do 
	-- OT.lstarLoop T (A 0) $ OT.mkTable (A 0)
	OT.lstarLoop JO.JsonTeacher (JA.Control JA.Null) $ OT.mkTable (JA.Control JA.Null) 
	-- OT.lstarLoop T (52 :: Int) $ OT.mkTable (52 :: Int)
	putStrLn "Done"