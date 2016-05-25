{-# LANGUAGE OverloadedStrings #-}
module Poseidon.MPD (play, pause, stop, getCurPL, getLib, waitForPL) where

import qualified Network.MPD as MPD
import qualified Poseidon.DSL as P

import Data.Maybe (listToMaybe, fromJust, fromMaybe)

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
  errOrSongs <- MPD.withMPD_ stdMpdHost stdMpdPort $ MPD.playlistInfo Nothing
  curSong <- MPD.withMPD_ stdMpdHost stdMpdPort $ MPD.currentSong
  let idx = either (const (-1)) (fromMaybe (-1) . MPD.sgIndex . fromJust) curSong 
  case errOrSongs of
    Left err -> fail $ show err
    Right s -> return $ map (toPSong idx) s
  where 
    toPSong idx mpdSong = P.Song { P._artist  = get MPD.Artist
                             , P._title   = get MPD.Title
                             , P._album   = get MPD.Album
                             , P._length  = length
                             , P._playing = playing
                             }
      where
        length = MPD.sgLength mpdSong
        get x = do
          val <- MPD.sgGetTag x mpdSong
          val' <- listToMaybe val
          return $ MPD.toString val'
        playing = (Just idx == MPD.sgIndex mpdSong)

waitForPL :: IO ()
waitForPL = wrapMPD $ MPD.idle [MPD.PlaylistS]

getLib :: IO [P.Song]
getLib = undefined

