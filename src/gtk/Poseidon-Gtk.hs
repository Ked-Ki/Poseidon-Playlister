module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Lens
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Control.Monad (forever)

import qualified Poseidon.MPD as MPD
import qualified Poseidon.DSL as DSL

main = do
  -- GUI setup
  initGUI
  builder <- builderNew
  builderAddFromFile builder "poseidon_front.glade"

  -- bind GUI objects
  window <- builderGetObject builder castToWindow "window1"
  playButton <- builderGetObject builder castToButton "playButton"
  pauseButton <- builderGetObject builder castToButton "pauseButton"
  stopButton <- builderGetObject builder castToButton "stopButton"
  curPlaylistView <- builderGetObject builder castToTreeView "curView"
  playlistStore <- mkSongListStore curPlaylistView

  -- do it once, nonAsync
  pl <- MPD.getCurPL
  mapM_ (listStoreAppend playlistStore) pl
  forkIO $ fetchPlaylist playlistStore

  -- button handlers
  playButton `onClicked` MPD.play 
  pauseButton `onClicked` MPD.pause
  stopButton `onClicked` MPD.stop
  onDestroy window mainQuit

  -- run GUI
  widgetShowAll window
  mainGUI

mkSongListStore :: TreeView -> IO (ListStore DSL.Song)
mkSongListStore v = do
  playlistStore <- listStoreNew ([] :: [DSL.Song])
  addTextColumn v playlistStore (fetchInfo (DSL.artist . cleanInfo)) "Artist"
  addTextColumn v playlistStore (fetchInfo (DSL.title . cleanInfo)) "Title"
  addTextColumn v playlistStore (fetchInfo (DSL.album . cleanInfo)) "Album"
  addTextColumn v playlistStore (fetchInfo (DSL.length . to timeStr)) "Length"
  treeViewSetModel v playlistStore
  return playlistStore
    where 
      addTextColumn view model f name = do
        col <- treeViewColumnNew
        rend <- cellRendererTextNew
        treeViewColumnSetTitle col name
        treeViewColumnPackStart col rend True
        cellLayoutSetAttributes col rend model f
        treeViewColumnSetExpand col True
        treeViewAppendColumn view col

      fetchInfo lens song = [ cellText := (song ^. lens)
                            , cellTextWeightSet := song ^. DSL.playing
                            , cellTextWeight :~ (+100)
                            ]
      cleanInfo = to $ fromMaybe "<empty>"

timeStr :: Integer -> String
timeStr x = showTimeMin ++ ":" ++ showTimeSec
  where
    showTimeMin = show $ x `div` 60
    timeSec = x `mod` 60
    showTimeSec = if timeSec < 10 
                  then "0" ++ show timeSec
                  else show timeSec

--- handler functions
fetchPlaylist :: ListStore DSL.Song -> IO ()
fetchPlaylist listStore = forever $ do
  pl <- MPD.getCurPL
  postGUIAsync $ do
    listStoreClear listStore
    mapM_ (listStoreAppend listStore) pl
  MPD.waitForPL
