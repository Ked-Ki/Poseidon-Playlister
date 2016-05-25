module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Lens
import Data.Maybe (fromMaybe)

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

  playlistStore <- listStoreNew ([] :: [DSL.Song])

  addTextColumn curPlaylistView playlistStore ((fromMaybe "") . (view DSL.artist)) "Artist"
  addTextColumn curPlaylistView playlistStore ((fromMaybe "") . (view DSL.title)) "Title"
  addTextColumn curPlaylistView playlistStore ((fromMaybe "") . (view DSL.album)) "Album"
  addTextColumn curPlaylistView playlistStore (showTime . view DSL.length) "Length"

  pl <- MPD.getCurPL
  mapM_ (listStoreAppend playlistStore) pl

  treeViewSetModel curPlaylistView playlistStore

  -- button handlers
  playButton `onClicked` MPD.play 
  pauseButton `onClicked` MPD.pause
  stopButton `onClicked` MPD.stop
  onDestroy window mainQuit

  -- run GUI
  widgetShowAll window
  mainGUI

addTextColumn view model f name =
  do
  col <- treeViewColumnNew
  rend <- cellRendererTextNew
  treeViewColumnSetTitle col name
  treeViewColumnPackStart col rend True
  cellLayoutSetAttributes col rend model (\row -> [ cellText := f row ])

  treeViewColumnSetExpand col True
  treeViewAppendColumn view col

showTime :: Integer -> String
showTime x = showTimeMin ++ ":" ++ showTimeSec
  where
    showTimeMin = show $ x `div` 60
    showTimeSec = show $ x `mod` 60
