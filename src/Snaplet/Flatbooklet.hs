{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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

type FlatHandler r = forall a. Handler a (Flatbooklet a) r

unauthorized :: FlatHandler ()
unauthorized = do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    writeText "User not logged in."


-- | Wraps given handler with checks to ensure
-- a) a logged in user and
-- b) that the specified user in URL matches the logged in user
protect :: FlatHandler () -- ^ void handler to protect
        -> FlatHandler ()
protect action = (flatbookletAuth <$> get) >>= \auth -> requireUser auth unauthorized $ do
    withLogin $ \login -> do 
        urlUser <- fmap decodeUtf8 <$> getParam "user"
        case urlUser of
            Nothing -> unauthorized 
            Just userName -> if userName == login
                                then action
                                else unauthorized -- hack attempt

-- | Serve doc with given id, lookup from user's documents.
-- Returns HTTP 400 "Bad request" if id missing (routing prevents this)
-- Returns HTTP 404 "Not found" if document not found
getDoc :: FlatHandler ()
getDoc = withUserCache $ \cache -> do
    idm <- getParam "id"
    case idm of
        Just ids -> writeBS $ "TODO: Document with id: " `B.append` ids
        Nothing  -> modifyResponse $ setResponseStatus 400 "Bad request"

-- | Serve all user's documents in JSON list containing
-- { id: ID, doc: DOC } doc ids and documents truncated
-- to 100 characters.
getDocs :: FlatHandler ()
getDocs = withUserCache $ \cache -> do
    -- FIXME: format as specs say
    writeText . T.pack . show $ cache


-- | Return user document overall statistics as JSON
-- document.
getStats :: FlatHandler ()
getStats = withUserCache $ \cache -> do
    writeText . T.pack $ show (length cache) ++ " documents"


-- | Returns login of the current user logged in.
-- NOTE: Logged in user is assumed.
withLogin action = (flatbookletAuth <$> get) >>= \auth -> do
    l <- fmap userLogin <$> withTop auth currentUser
    case l of
        Just login -> action login
        Nothing    -> unauthorized

-- | Perform tasks associated to user login. Load documents and
-- create indices.
atLogin :: FlatHandler ()
atLogin = do
    ioStuff <- return ["sampledata","asd"]
    modifyUserCache (\_ -> ioStuff)

-- | Perform tasks associated to user logout. Forget all kept stuff.
atLogout :: FlatHandler ()
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
withUserCache :: ([Doc] -> FlatHandler ())
              -> FlatHandler ()
withUserCache handler = withLogin $ \l -> do
    v <- cacheTVar
    allCache <- liftIO $ readTVarIO v
    let cache :: [Doc] = fromJust . M.lookup l $ allCache
    handler cache

modifyUserCache :: ([Doc] -> [Doc]) -- ^ action to perform on user docs
                -> FlatHandler ()
modifyUserCache action = withLogin $ \l -> do
    v <- cacheTVar
    liftIO . atomically $ do
        cache <- readTVar v
        let userdocs = fromMaybe [] $ M.lookup l cache
        -- Data.Map.insert adds or replaces (if needed) previous contents
        modifyTVar' v $ M.insert l (action userdocs)

rmUserCache :: FlatHandler ()
rmUserCache = withLogin $ \l -> do
    v <- cacheTVar
    liftIO . atomically $ modifyTVar' v (\m -> M.delete l m)

cacheTVar :: FlatHandler (TVar (M.Map Text [Doc]))
cacheTVar = userData <$> get
