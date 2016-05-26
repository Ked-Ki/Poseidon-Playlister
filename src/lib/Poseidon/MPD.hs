{-# LANGUAGE OverloadedStrings #-}
module Poseidon.MPD (play, pause, stop, next, prev,
                     getCurPL, getLib, waitForPL,
                     addSong, deleteSong) 
  where

import qualified Network.MPD as MPD
import qualified Poseidon.DSL as P

import Data.Maybe (listToMaybe, fromJust, fromMaybe)
import Control.Monad (join)
import Control.Lens

stdMpdHost = Just "lenore.kedki.me" -- test values for dev
stdMpdPort = Just "6600"

play :: IO ()
play = wrapMPD $ MPD.play Nothing 

pause :: IO ()
pause = wrapMPD $ MPD.pause True

stop :: IO ()
stop = wrapMPD MPD.stop

next :: IO ()
next = wrapMPD $ MPD.next

prev :: IO ()
prev = wrapMPD $ MPD.previous

addSong :: P.Song -> IO ()
addSong song = wrapMPD $ MPD.add (song ^. P.path)

deleteSong :: P.Song -> IO ()
deleteSong song = deleteSong' $ song ^. P.id
  where
    deleteSong' (Just idx) = wrapMPD $ MPD.deleteId idx
    deleteSong' Nothing = return ()

wrapMPD :: MPD.MPD a -> IO ()
wrapMPD ma = do
  MPD.withMPD_ stdMpdHost stdMpdPort ma
  return ()

getCurPL :: IO [P.Song]
getCurPL = do 
  errOrSongs <- MPD.withMPD_ stdMpdHost stdMpdPort $ MPD.playlistInfo Nothing
  curSong <- MPD.withMPD_ stdMpdHost stdMpdPort $ MPD.currentSong
  let idx = case curSong of
        Right (Just song) -> fromMaybe (-1) $ MPD.sgIndex song
        _ -> (-1)
  case errOrSongs of
    Left err -> fail $ show err
    Right s -> return $ map (toPSong idx) s

waitForPL :: IO ()
waitForPL = wrapMPD $ MPD.idle [MPD.PlaylistS, MPD.PlayerS]

getLib :: IO [P.Song]
getLib = do
  results <- MPD.withMPD_ stdMpdHost stdMpdPort (MPD.listAllInfo "")
  let allSongs = concatMap (filter isSong) results
  return $ map (toPSong (-1) . getSong) allSongs
    where
      getSong (MPD.LsSong s) = s
      isSong (MPD.LsSong _) = True
      isSong _ = False

toPSong :: Int -> MPD.Song -> P.Song
toPSong idx mpdSong = P.Song { P._artist  = get MPD.Artist
                             , P._title   = get MPD.Title
                             , P._album   = get MPD.Album
                             , P._length  = length
                             , P._playing = playing
                             , P._path = MPD.sgFilePath mpdSong
                             , P._id = MPD.sgId mpdSong
                             }
  where
    length = MPD.sgLength mpdSong
    get x = do
      val <- MPD.sgGetTag x mpdSong
      val' <- listToMaybe val
      return $ MPD.toString val'
    playing = (Just idx == MPD.sgIndex mpdSong)
