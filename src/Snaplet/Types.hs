{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Snaplet.Types where

import Control.Lens hiding ((.=))
import Control.Concurrent.STM

import Snap.Snaplet
import Snap.Core
import Snap.Snaplet.Auth
import Data.Aeson as JSON

import qualified Data.Text as T
import Data.Time
import Data.Text
import qualified Data.Map as M

data Doc = Doc
    { _text  :: Text
    , _times :: [UTCTime] }

makeLenses ''Doc

instance ToJSON Doc where
    toJSON doc = object $ ["note"  .= view text doc
                          ,"times" .= view times doc]

instance Show Doc where
    show = T.unpack . T.take 100 . view text

type SHA1 = String

data UserCache = UserCache
    { _state :: SHA1
    , _docs  :: M.Map SHA1 Doc }

makeLenses ''UserCache

instance Eq UserCache where
    c1 == c2 = view state c1 == view state c2

instance Show UserCache where
    show r = "Cache "++show (view state r)++" with "++show (M.size (_docs r))++ " documents."

type Caches = M.Map Text (TVar UserCache)
