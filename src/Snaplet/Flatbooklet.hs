{-# LANGUAGE OverloadedStrings #-}

module Snaplet.Flatbooklet where

import Snap.Snaplet
import Snap.Core
import Snap.Snaplet.Auth

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.STM

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe

import qualified Data.Map as M
import qualified Data.ByteString as B

import Snaplet.Types

data Flatbooklet a = Flatbooklet
    { flatbookletAuth :: SnapletLens a (AuthManager a)
    , userData        :: TVar (M.Map Text [Doc])
    , dataDir         :: FilePath }

deny :: Handler a (Flatbooklet a) ()
deny = do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    writeText "User not logged in."

-- | Wraps given handler with checks to ensure
-- a) a logged in user and
-- b) that the specified user in URL matches the logged in user
protect :: Handler a (Flatbooklet a) () -- ^ void handler to protect
        -> Handler a (Flatbooklet a) ()
protect action = (flatbookletAuth <$> get) >>= \auth -> requireUser auth deny $ do
    login <- getLogin
    urlUser <- fmap decodeUtf8 <$> getParam "user"
    case urlUser of
        Nothing -> deny -- invalid URL
        Just userName -> if userName == login -- hack attempt
                            then action
                            else deny

-- | Serve doc with given id, lookup from user's documents.
-- Returns HTTP 400 "Bad request" if id missing (routing prevents this)
-- Returns HTTP 404 "Not found" if document not found
getDoc :: Handler a (Flatbooklet a) ()
getDoc = withDocs $ do
    idm <- getParam "id"
    case idm of
        Just ids -> writeBS $ "Document with id: " `B.append` ids
        Nothing ->  do
            modifyResponse $ setResponseStatus 400 "Bad request"

-- | Serve all user's documents in JSON list containing
-- { id: ID, doc: DOC } doc ids and documents truncated
-- to 100 characters.
getDocs :: Handler a (Flatbooklet a) [Doc]
getDocs = do
    login <- getLogin
    datavar <- userData <$> get
    d <- M.lookup login <$> (liftIO $ readTVarIO datavar)
    case d of
        Just docs -> return docs
        Nothing   -> return ["error"]

-- | Return user document overall statistics as JSON
-- document.
getStats :: Handler a (Flatbooklet a) ()
getStats = withDocs $ do
    docs <- getDocs
    writeText . T.pack $ show (length docs) ++ " documents"


-- | Returns login of the current user logged in.
-- NOTE: Logged in user is assumed.
getLogin :: Handler a (Flatbooklet a) Text
getLogin = (flatbookletAuth <$> get) >>= \auth -> do
    (userLogin . fromJust) <$> withTop auth currentUser
    
-- |Wrapper that loads documents for current user if needed.
-- FIXME: simply do this only at user login
-- FIXME: rename
withDocs :: Handler a (Flatbooklet a) () -> Handler a (Flatbooklet a) ()
withDocs action = do
    login <- getLogin
    datavar <- userData <$> get
    liftIO . atomically $ do
        userData <- readTVar datavar
        if M.member login userData
            then return ()
            else modifyTVar' datavar (\m -> M.insert login ["sampledata","asd"] m)
    action     

--flatbookletHandleLogin :: Handler a (Flatbooklet a) ()



flatbookletInit :: SnapletLens a (AuthManager a) -> SnapletInit a (Flatbooklet a)
flatbookletInit authLens = makeSnaplet "Flatbooklet" "Flatbooklet git backend" Nothing $ do
    addRoutes [ ("/hello", writeText "hello world with snaplets!" )
              , ("/:user/get/:id", protect getDoc)
              , ("/:user/stats", protect getStats)
              ]
    docs <- liftIO $ newTVarIO M.empty
    return $ Flatbooklet authLens docs path
  where path = "data" -- TODO: read from config
