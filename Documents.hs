{-# LANGUAGE ScopedTypeVariables #-}
module Documents where

import System.Directory
import System.FilePath.Posix
import Control.Applicative
import Control.Monad
import Control.Arrow
import Lib.Git as G

import Search
import Types

load :: FilePath -> IO [Doc]
load dir = do
   all <- getDirectoryContents dir
   -- filter out directories (".", "..", ...) from results
   files <- filterM (doesFileExist . toPath) all
   docs <- mapM (readFile . toPath) files
   return docs
  where toPath fn = dir ++ "/" ++ fn


-- |Return SHA1 of the current commit in given dir
currentSHA1 :: FilePath -> IO (Either String SHA1)
currentSHA1 dir = do
    out <- G.runGit (G.Config dir Nothing) $ G.gitExec "rev-parse" ["HEAD"] []
    -- Make possible GitError in Left to String with show
    return . left show . right (SHA1 . head . lines) $ out
 
