{-# LANGUAGE TemplateHaskell #-}
module Poseidon.DSL where

import Control.Lens

data Song = Song { _artist :: Maybe String
                 , _title :: Maybe String
                 , _album :: Maybe String
                 , _length :: Integer -- Seconds
                 }
 deriving Show

makeLenses ''Song
