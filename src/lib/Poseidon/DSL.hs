{-# LANGUAGE TemplateHaskell #-}
module Poseidon.DSL where

import Control.Lens
import qualified Network.MPD as MPD

data Song = Song { _artist :: Maybe String
                 , _title :: Maybe String
                 , _album :: Maybe String
                 , _length :: Integer -- Seconds
                 , _playing :: Bool
                 , _path :: MPD.Path
                 , _id :: Maybe MPD.Id
                 }
 deriving (Show,Eq)

makeLenses ''Song

