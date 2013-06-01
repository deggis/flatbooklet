{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Core

import Snaplet.Flatbooklet

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

