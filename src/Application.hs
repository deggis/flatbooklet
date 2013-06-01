{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Core
import Data.Text.Encoding
import Data.Text
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString as B

import Types

data Flatbooklet a = Flatbooklet { flatbookletAuth :: SnapletLens a (AuthManager a)
                                 , userData        :: MVar (M.Map Text [Doc])
                                 , dataDir         :: FilePath }
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _db   :: Snaplet (Flatbooklet App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------
type AppHandler = Handler App App

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
