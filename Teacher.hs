module LearnJSON.Teacher (
	Teacher,
	IOTeacher(..),
	member
	) where

import qualified LearnJSON.Tree as T 

class Teacher teach where 
	member :: (Show b) => teach b -> (T.Tree b) -> IO Bool 

data IOTeacher a = IOTeacher

-- | used to ask whether this tree is a member of the language
ioMember :: (Show a) => IOTeacher a -> T.Tree a -> IO Bool 
ioMember _ tree = do 
	putStrLn "memb?"
	putStrLn $ show tree 
	result <- getLine 
	return (result == "y")

instance Teacher IOTeacher where
	member = ioMember