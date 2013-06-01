{-# LANGUAGE OverloadedStrings #-}

module Snaplet.Flatbooklet where

import Snap.Snaplet
import Snap.Core
import Snap.Snaplet.Auth

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.MVar

import Data.Text
import Data.Text.Encoding
import Data.Maybe

import qualified Data.Map as M
import qualified Data.ByteString as B

import Snaplet.Types

data Flatbooklet a = Flatbooklet { flatbookletAuth :: SnapletLens a (AuthManager a)
                                 , userData        :: MVar (M.Map Text [Doc])
                                 , dataDir         :: FilePath }

deny :: Handler a (Flatbooklet a) ()
deny = do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    writeText "User not logged in."


protect :: Handler a (Flatbooklet a) () -> Handler a (Flatbooklet a) ()
protect action = (flatbookletAuth <$> get) >>= \auth -> requireUser auth deny $ do
    login <- getUser 
    urlUser <- fmap decodeUtf8 <$> getParam "user"
    case urlUser of
        Nothing -> deny -- invalid URL
        Just userName -> if userName == login -- hack attempt
                            then action
                            else deny

getDoc :: Handler a (Flatbooklet a) ()
getDoc = withDocs $ do
    idm <- getParam "id"
    case idm of
        Just ids -> writeBS $ "Document with id: " `B.append` ids
        Nothing ->  do
            modifyResponse $ setResponseStatus 400 "Bad request"

-- |Returns login of the current user logged in.
-- NOTE: Logged in user is assumed.
getUser :: Handler a (Flatbooklet a) Text
getUser = (flatbookletAuth <$> get) >>= \auth -> do
    (userLogin . fromJust) <$> withTop auth currentUser
    
-- |Wrapper that loads documents for current user if needed.
withDocs :: Handler a (Flatbooklet a) () -> Handler a (Flatbooklet a) ()
withDocs action = do
    login <- getUser
    action

getStats :: Handler a (Flatbooklet a) ()
getStats = withDocs $ writeText "0 documents"

flatbookletInit :: SnapletLens a (AuthManager a) -> SnapletInit a (Flatbooklet a)
flatbookletInit authLens = makeSnaplet "Flatbooklet" "Flatbooklet git backend" Nothing $ do
    addRoutes [ ("/hello", writeText "hello world with snaplets!" )
              , ("/:user/get/:id", protect getDoc)
              , ("/:user/stats", protect getStats)
              ]
    docs <- liftIO $ newMVar M.empty
    liftIO $ putStrLn "Initializing snaplet"
    return $ Flatbooklet authLens docs path
  where path = "data" -- TODO: read from config
