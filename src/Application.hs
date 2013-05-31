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
import Control.Monad.IO.Class
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Core
import Data.Text.Encoding
import Data.Maybe
import qualified Data.ByteString as B

data Flatbooklet a = Flatbooklet { flatbookletAuth :: SnapletLens a (AuthManager a) }
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
    login <- (userLogin . fromJust) <$> withTop auth currentUser
    urlUser <- fmap decodeUtf8 <$> getParam "user"
    case urlUser of
        Nothing -> deny -- invalid URL
        Just userName -> if userName == login -- hack attempt
                            then action
                            else deny

getDoc :: Handler a (Flatbooklet a) ()
getDoc = do
    idm <- getParam "id"
    case idm of
        Just ids -> writeBS $ "Document with id: " `B.append` ids
        Nothing ->  do
            modifyResponse $ setResponseStatus 400 "Bad request"

getStats :: Handler a (Flatbooklet a) ()
getStats = writeText "0 documents"

flatbookletInit :: SnapletLens a (AuthManager a) -> SnapletInit a (Flatbooklet a)
flatbookletInit authLens = makeSnaplet "Flatbooklet" "Flatbooklet git backend" Nothing $ do
    addRoutes [ ("/hello", writeText "hello world with snaplets!" )
              , ("/:user/get/:id", protect getDoc)
              , ("/:user/stats", protect getStats)
              ]
    liftIO $ putStrLn "Initializing snaplet"
    return $ Flatbooklet authLens
