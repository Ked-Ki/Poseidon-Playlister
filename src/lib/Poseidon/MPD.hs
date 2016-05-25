{-# LANGUAGE OverloadedStrings #-}
module Poseidon.MPD (play, pause, stop, getCurPL, getLib) where

import qualified Network.MPD as MPD
import qualified Poseidon.DSL as P

import Data.Maybe (listToMaybe)

stdMpdHost = Just "lenore" -- test values for dev
stdMpdPort = Just "6600"

play :: IO ()
play = wrapMPD $ MPD.play Nothing

pause :: IO ()
pause = wrapMPD $ MPD.pause True

stop :: IO ()
stop = wrapMPD MPD.stop

wrapMPD :: MPD.MPD a -> IO ()
wrapMPD ma = do
  MPD.withMPD_ stdMpdHost stdMpdPort ma
  return ()

getCurPL :: IO [P.Song]
getCurPL = do
    errOrSongs <- MPD.withMPD_ stdMpdHost stdMpdPort $ 
      MPD.playlistInfo Nothing
    case errOrSongs of
      Left err -> fail $ show err
      Right s -> return $ map toPSong s
  where 
    toPSong mpdSong = P.Song { P._artist = get MPD.Artist
                             , P._title  = get MPD.Title
                             , P._album  = get MPD.Album
                             , P._length = length
                             }
      where
        length = MPD.sgLength mpdSong
        get x = do
          val <- MPD.sgGetTag x mpdSong
          val' <- listToMaybe val
          return $ MPD.toString val'

getLib :: IO [P.Song]
getLib = undefined

