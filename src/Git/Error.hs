{-# LANGUAGE DeriveDataTypeable #-}

module Git.Error where
import Control.Exception
import Data.Typeable

data Error = Error { error_code::Int
                   , error_explanation::String
                   }
           deriving (Show, Typeable)

instance Exception Error