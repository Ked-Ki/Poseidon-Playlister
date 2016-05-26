module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import Control.Monad (forever)
import Data.List ((\\),nubBy)

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
  nextButton <- builderGetObject builder castToButton "nextButton"
  prevButton <- builderGetObject builder castToButton "prevButton"
  pauseButton <- builderGetObject builder castToButton "pauseButton"
  stopButton <- builderGetObject builder castToButton "stopButton"

  curPlaylistView <- builderGetObject builder castToTreeView "curView"
  libView <- builderGetObject builder castToTreeView "libView"
  playlistStore <- mkSongListStore curPlaylistView
  libStore <- mkSongListStore libView

  filterButton <- builderGetObject builder castToButton "filterButton"
  nubButton <- builderGetObject builder castToButton "nubButton"
  deleteButton <- builderGetObject builder castToButton "deleteButton"

  addAllButton <- builderGetObject builder castToButton "addAllButton"

  artistLensToggle <- builderGetObject builder castToToggleButton "artistLens"
  albumLensToggle <- builderGetObject builder castToToggleButton "albumLens"
  titleLensToggle <- builderGetObject builder castToToggleButton "titleLens"

  plMVar <- newEmptyMVar
  selMVar <- newMVar (==)

  lib <- MPD.getLib
  mapM_ (listStoreAppend libStore) lib
  pl <- MPD.getCurPL
  putMVar plMVar pl
  mapM_ (listStoreAppend playlistStore) pl
  forkIO $ fetchPlaylist playlistStore plMVar

  -- button handlers
  on artistLensToggle toggled (checkLensToggles artistLensToggle albumLensToggle 
                                               titleLensToggle selMVar)
  on albumLensToggle toggled (checkLensToggles artistLensToggle albumLensToggle 
                                               titleLensToggle selMVar)
  on titleLensToggle toggled (checkLensToggles artistLensToggle albumLensToggle 
                                               titleLensToggle selMVar)
  playButton `onClicked` MPD.play 
  nextButton `onClicked` MPD.next
  prevButton `onClicked` MPD.prev
  pauseButton `onClicked` MPD.pause
  stopButton `onClicked` MPD.stop

  let addBy f = do {
      song <- getCurSelSong libView lib;
      eqFunc <- readMVar selMVar;
      case song of 
          Just s -> mapM_ MPD.addSong $ (f eqFunc s) lib
          _ -> return ()}

  let deleteBy f = do {
      curPl <- readMVar plMVar;
      eqFunc <- readMVar selMVar;
      song <- getCurSelSong curPlaylistView curPl;
      case song of
        Just s -> mapM_ MPD.deleteSong $ (f eqFunc s) curPl
        _ -> return ()}

  filterButton `onClicked` deleteBy (\f s -> filter (not . f s))
  nubButton `onClicked` deleteBy (\f s pl -> pl \\ (nubBy f pl))
  deleteButton `onClicked` deleteBy (\f s -> filter (f s))
  addAllButton `onClicked` addBy (\f s -> filter (f s))

  onDestroy window mainQuit

  -- run GUI
  widgetShowAll window
  mainGUI
    where
      getCurSelSong view list = do
        (tPath, _) <- treeViewGetCursor view
        let idx = listToMaybe tPath
        return $ (list !!) <$> idx
      checkLensToggles arLT alLT tiLT selMVar = do
        artistBool <- toggleButtonGetActive arLT
        albumBool <- toggleButtonGetActive alLT
        titleBool <- toggleButtonGetActive tiLT

        func <- takeMVar selMVar
        
        let eqFunc a b = (artistBool && a ^. DSL.artist == b ^. DSL.artist) ||
                         (albumBool && a ^. DSL.album == b ^. DSL.album) ||
                         (titleBool && a ^. DSL.title == b ^. DSL.title) ||
                         (a == b)

        putMVar selMVar eqFunc 




mkSongListStore :: TreeView -> IO (ListStore DSL.Song)
mkSongListStore v = do
  plStore <- listStoreNew ([] :: [DSL.Song])
  addTextColumn plStore (DSL.artist . cleanInfo)  "Artist"
  addTextColumn plStore (DSL.title . cleanInfo)   "Title"
  addTextColumn plStore (DSL.album . cleanInfo)   "Album"
  addTextColumn plStore (DSL.length . to timeStr) "Length"
  treeViewSetModel v plStore
  return plStore
    where 
      addTextColumn model lens name = do
        col <- treeViewColumnNew
        rend <- cellRendererTextNew
        treeViewColumnSetTitle col name
        treeViewColumnSetResizable col True
        treeViewColumnPackStart col rend True
        cellLayoutSetAttributes col rend model (fetchInfo lens)
        treeViewColumnSetExpand col True
        treeViewAppendColumn v col

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
fetchPlaylist :: ListStore DSL.Song -> MVar [DSL.Song] -> IO ()
fetchPlaylist listStore plMVar = forever $ do
  takeMVar plMVar
  pl <- MPD.getCurPL
  putMVar plMVar pl
  postGUIAsync $ do
    listStoreClear listStore
    mapM_ (listStoreAppend listStore) pl
  MPD.waitForPL
