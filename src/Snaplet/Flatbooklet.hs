{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Snaplet.Flatbooklet where

import Snap.Snaplet
import Snap.Core
import Snap.Snaplet.Auth

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.STM

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe
import Data.Aeson as JSON

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Snaplet.Types
import Snaplet.UserCache


data Flatbooklet a = Flatbooklet
    { flatbookletAuth :: SnapletLens a (AuthManager a)
    , caches          :: TVar Caches
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
protect action = (flatbookletAuth <$> get) >>= \auth -> requireUser auth unauthorized $
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
getDocs = withUserCache $ \cache ->
    writeBS . toStrict . JSON.encode $ view docs cache

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

writeJSON = writeBS . toStrict . JSON.encode

-- | Return user document overall statistics as JSON
-- document.
getStats :: FlatHandler ()
getStats = withUserCache $ \cache ->
    writeText . T.pack $ show cache


-- | Wrap given 'handler', feed it with user login 
withLogin action = (flatbookletAuth <$> get) >>= \auth -> do
    l <- fmap userLogin <$> withTop auth currentUser
    case l of
        Just login -> action login
        Nothing    -> unauthorized

-- | Perform tasks associated to user login. Load documents and
-- create indices.
atLogin :: FlatHandler ()
atLogin = withLogin $ \l -> do
    res <- liftIO $ loadUserCache ("./data/"++(T.unpack l))
    case res of
        Left s      -> error s -- TODO: error page
        Right cache -> initUserCache cache

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
    cache <- liftIO . newTVarIO $ M.empty 
    return $ Flatbooklet authLens cache path
  where path = "data" -- TODO: read from config






-- *UserCache* functions handle tasks of normal session management.
-- Could be later replaced with snaplet-typed-sessions or some other
-- Snaplet session mechanism that allows typed session variables.


withUserCache :: (UserCache -> FlatHandler ())
              -> FlatHandler ()
withUserCache handler = withUserCacheTVar $ (liftIO . readTVarIO) >=> handler


withUserCacheTVar :: (TVar UserCache -> FlatHandler ())
              -> FlatHandler ()
withUserCacheTVar handler = withLogin $ \l -> do
    v <- cachesTVar
    caches :: Caches <- liftIO . readTVarIO $ v
    
    -- 'fromJust': the use of withLogin wrapper
    -- ensures that Map contains given login
    case (M.lookup l caches) of
        Just c -> handler c
        _      -> error "Cache miss in withLogin wrapped handler!"

initUserCache :: UserCache -> FlatHandler ()
initUserCache userCache = withLogin $ \l -> do
    cachesT <- cachesTVar
    liftIO . atomically $ do
        userT <- newTVar userCache
        modifyTVar' cachesT (M.insert l userT)

modifyUserCache :: (UserCache -> UserCache) -- ^ action to perform on user docs
                -> FlatHandler ()
modifyUserCache action =
    withLogin $ \l ->
        withUserCacheTVar $ \cv ->
            liftIO . atomically $ modifyTVar' cv action

-- | Removes UserCache from Cache.
-- Just removes entry from Map, leaving (TVar Cache) to GC's mercy.
rmUserCache :: FlatHandler ()
rmUserCache = withLogin $ \l -> do
    v <- cachesTVar
    liftIO . atomically $ modifyTVar' v (M.delete l)

cachesTVar :: FlatHandler (TVar Caches)
cachesTVar = caches <$> get
