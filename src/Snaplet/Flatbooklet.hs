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
        Just userName -> if userName == login
                            then action
                            else deny -- hack attempt

-- | Serve doc with given id, lookup from user's documents.
-- Returns HTTP 400 "Bad request" if id missing (routing prevents this)
-- Returns HTTP 404 "Not found" if document not found
getDoc :: Handler a (Flatbooklet a) ()
getDoc = do 
    idm <- getParam "id"
    cache <- readUserCache
    case idm of
        Just ids -> writeBS $ "TODO: Document with id: " `B.append` ids
        Nothing  -> modifyResponse $ setResponseStatus 400 "Bad request"

-- | Serve all user's documents in JSON list containing
-- { id: ID, doc: DOC } doc ids and documents truncated
-- to 100 characters.
getDocs :: Handler a (Flatbooklet a) ()
getDocs = do
    cache <- readUserCache
    -- FIXME: format as specs say
    writeText . T.pack . show $ cache


-- | Return user document overall statistics as JSON
-- document.
getStats :: Handler a (Flatbooklet a) ()
getStats = undefined --withDocs -- $ d
--    docs <- getDocs
--    writeText . T.pack $ show (length docs) ++ " documents"


-- | Returns login of the current user logged in.
-- NOTE: Logged in user is assumed.
getLogin :: Handler a (Flatbooklet a) Text
getLogin = (flatbookletAuth <$> get) >>= \auth -> do
    (userLogin . fromJust) <$> withTop auth currentUser
    


-- | Perform tasks associated to user login. Load documents and
-- create indices.
atLogin :: Handler a (Flatbooklet a) ()
atLogin = do 
    login <- getLogin
    datavar <- userData <$> get
    liftIO . atomically $ do
        userData <- readTVar datavar
        if M.member login userData
            then return ()
            else modifyTVar' datavar (\m -> M.insert login ["sampledata","asd"] m)

-- | Perform tasks associated to user logout. Forget all kept stuff.
atLogout :: Handler a (Flatbooklet a) ()
atLogout = rmUserCache

flatbookletInit :: SnapletLens a (AuthManager a) -> SnapletInit a (Flatbooklet a)
flatbookletInit authLens = makeSnaplet "Flatbooklet" "Flatbooklet git backend" Nothing $ do
    addRoutes [ ("/hello", writeText "hello world with snaplets!" )
              , ("/:user/get/:id", protect getDoc)
              , ("/:user/all",     protect getDocs)
              , ("/:user/stats",   protect getStats)
              ]
    docs <- liftIO $ newTVarIO M.empty
    return $ Flatbooklet authLens docs path
  where path = "data" -- TODO: read from config




-- cache poking helpers
        
readUserCache = do
    (l,v) <- loginAndCacheTVar
    cache <- liftIO $ readTVarIO v
    return . fromJust . M.lookup l $ cache

withUserCache action = do
    (l,v) <- loginAndCacheTVar
    liftIO . atomically $ do
        cache <- readTVar v
        let userdocs = fromMaybe [] $ M.lookup l cache
        -- Data.Map.insert adds or replaces (if needed) previous contents
        modifyTVar' v $ M.insert l (action userdocs)

rmUserCache = do
    (l,v) <- loginAndCacheTVar
    liftIO . atomically $ modifyTVar' v (\m -> M.delete l m)

loginAndCacheTVar = do
    login <- getLogin
    cvar <- userData <$> get
    return (login,cvar)
