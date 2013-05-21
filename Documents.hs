{-# LANGUAGE ScopedTypeVariables #-}
module Documents where

import System.Directory
import System.FilePath.Posix
import Control.Applicative
import Control.Monad

load :: FilePath -> IO [Doc]
load dir = do
   all <- getDirectoryContents dir
   files <- filterM (doesFileExist . toPath) all
   docs <- mapM (readFile . toPath) files
   return docs
  where toPath fn = dir ++ "/" ++ fn

