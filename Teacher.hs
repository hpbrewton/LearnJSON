module LearnJSON.Teacher (
	Teacher,
	IOTeacher(..),
	member,
	equivalent
	) where

import qualified LearnJSON.Tree as T 

class Teacher teach where 
	member :: (Show b) => teach b -> (T.Tree b) -> IO Bool 
	equivalent :: (Read b) => teach b -> IO (Maybe (T.Tree b))

data IOTeacher a = IOTeacher

-- | used to ask whether this tree is a member of the language
ioMember :: (Show a) => IOTeacher a -> T.Tree a -> IO Bool 
ioMember _ tree = do 
	putStrLn "memb?"
	putStrLn $ show tree 
	result <- getLine 
	return (result == "y")

-- | used to ask for a possible counter-example 
ioEquivalent :: (Read a) => IOTeacher a -> IO (Maybe (T.Tree a))
ioEquivalent _ = do 
	putStrLn "equiv?"
	result <- getLine
	if result == "y"
		then return Nothing 
		else return $ Just $ read result

instance Teacher IOTeacher where
	member = ioMember
	equivalent = ioEquivalent