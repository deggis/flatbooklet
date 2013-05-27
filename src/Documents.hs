{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Documents where

import System.Directory
import System.FilePath.Posix
import Control.Applicative
import Control.Monad
import Control.Arrow
import Lib.Git as G

import Search
import Types


-- |Load files and close handles explicitly to
-- avoid having too many open handles
loadFiles :: [FilePath] -> IO [Doc]
loadFiles (f:files)


loadDocuments :: FilePath -> IO (Either String Repository)
loadDocuments dir = do
   all <- getDirectoryContents dir
   -- filter out directories (".", "..", ...) from results
   files <- filterM (doesFileExist . toPath) all
   docs <- loadFiles $ map toPath files
   sha1res <- currentSHA1 dir
   case sha1res of
       Left msg    -> return $ Left msg
       Right state -> return $ Right Repository{..}
  where
    toPath fn = dir ++ "/" ++ fn


-- |Return SHA1 of the current commit in given dir
currentSHA1 :: FilePath -> IO (Either String SHA1)
currentSHA1 dir = do
    out <- G.runGit (G.Config dir Nothing) $ G.gitExec "rev-parse" ["HEAD"] []
    -- Make possible GitError in Left to String with show
    return . left show . right (SHA1 . head . lines) $ out
 
