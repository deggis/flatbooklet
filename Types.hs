{-# LANGUAGE RecordWildCards #-}
module Types where

type Doc = String

newtype SHA1 = SHA1 String deriving(Eq,Show)

data Documents = Documents { state :: SHA1
                           , docs  :: [Doc] }

instance Eq Documents where
    Documents{state=s1} == Documents{state=s2} = s1 == s2
