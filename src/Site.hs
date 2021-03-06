{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application

import           Snaplet.Flatbooklet as F

-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


-- | Handle login submit. Perform given task handler if login successful
handleLoginSubmit :: Handler App (AuthManager App) () -- ^ login tasks handler
                  -> Handler App (AuthManager App) ()
handleLoginSubmit atLogin =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (atLogin >> redirect "/")
  where
    err = Just "Unknown user or password"


-- | Logs out and redirects the user to the site index,
-- performs given logout handler
handleLogout :: Handler App (AuthManager App) () -- ^ logout tasks handler
             -> Handler App (AuthManager App) ()
handleLogout atLogout = atLogout >> logout >> redirect "/"


-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


-- | The application's routes
routes :: Handler App (AuthManager App) ()
       -> Handler App (AuthManager App) ()
       -> [(ByteString, Handler App App ())]
routes atLogin atLogout =
    [ ("/login",    with auth $ handleLoginSubmit atLogin)
    , ("/logout",   with auth $ handleLogout atLogout)
    , ("/new_user", with auth handleNewUser)
    , ("",          serveDirectory "static")
    ]


-- | The application initializer
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    fbl <- nestSnaplet "fb" fb $ flatbookletInit auth
           
    addRoutes $ routes (withTop fb F.atLogin) (withTop fb F.atLogout)
    addAuthSplices h auth
    return $ App h s a fbl

