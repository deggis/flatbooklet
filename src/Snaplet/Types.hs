{-# LANGUAGE RecordWildCards #-}
module Snaplet.Types where

type Doc = String

newtype SHA1 = SHA1 String deriving(Eq)

instance Show SHA1 where
    show (SHA1 s) = "SHA1:"++s

data Repository = Repository { state :: SHA1
                             , docs  :: [Doc] }

instance Eq Repository where
    Repository{state=s1} == Repository{state=s2} = s1 == s2

instance Show Repository where
    show r = "Repository "++show (state r)++" with "++show (length (docs r))++ " documents."
