{-# LANGUAGE MultiParamTypeClasses #-}

module Teacher (
	Teacher,
	member,
	equivalent
	) where

import qualified Tree as T 
import qualified TreeAutomata as Aut

class Teacher teach b where 
	member :: (Show b) => teach -> (T.Tree b) -> IO Bool 
	equivalent :: (Show b, Read b) => teach -> Aut.TreeAutomata b -> IO (Maybe (T.Tree b))