{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Snaplet.UserCache where

import System.Directory
import System.FilePath.Posix

import Control.Applicative
import Control.Monad
import Control.Arrow
import Lib.Git as G
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
--import Snaplet.Search

import Snaplet.Types
import Snaplet.TimeParsers


loadUserCache :: FilePath -> IO (Either String UserCache)
loadUserCache dir = do
   sha1res <- currentSHA1 dir
   case sha1res of
       Left msg    -> return $ Left msg
       Right state -> do
           docs <- loadFiles
           return . Right $ UserCache state docs
  where
    toPath fn = dir ++ "/" ++ fn
    loadFiles = do
        all   <- getDirectoryContents dir        
        files <- filterM (doesFileExist . toPath) all -- filter out directories (".", "..", ...)
        docs  <- mapM (loadDoc . toPath) files
        return . M.fromList $ zip files docs

loadDoc :: FilePath -> IO Doc
loadDoc path = do
    text <- T.readFile path
    let times = parseTimes text
    return $ times `seq` Doc text times

-- |Return SHA1 of the current commit in given dir
currentSHA1 :: FilePath -> IO (Either String SHA1)
currentSHA1 dir = do
    out <- G.runGit (G.Config dir Nothing) $ G.gitExec "rev-parse" ["HEAD"] []
    -- Make possible GitError in Left to String with show
    return . left show . right (head . lines) $ out
