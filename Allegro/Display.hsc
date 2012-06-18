{-
 - Hurry is a Haskell binding to the Allegro game library.
 - Copyright (c) 2012 Damien Radtke - www.damienradtke.org
 -
 - This library is free software; you can redistribute it and/or
 - modify it under the terms of the GNU Lesser General Public
 - License as published by the Free Software Foundation; either
 - version 3.0 of the License, or (at your option) any later version.
 -
 - This library is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 - Lesser General Public License for more details.
 -
 - For more information, visit http://www.gnu.org/copyleft
 -}

module Allegro.Display where

#include <allegro5/allegro.h>

import Allegro.Graphics
import Allegro.Util

import Control.Monad
import Data.Bits
import Data.List
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data DisplayStruct
type Display = Ptr (DisplayStruct)

newtype DisplayFlag = DisplayFlag { unDisplayFlag :: CInt }
#{enum DisplayFlag, DisplayFlag
 , windowed = ALLEGRO_WINDOWED
 , fullscreen = ALLEGRO_FULLSCREEN
 , fullscreenWindow = ALLEGRO_FULLSCREEN_WINDOW
 , resizable = ALLEGRO_RESIZABLE
 , opengl = ALLEGRO_OPENGL
 , opengl3 = ALLEGRO_OPENGL_3_0
 , openglForwardCompatible = ALLEGRO_OPENGL_FORWARD_COMPATIBLE
 , noframe = ALLEGRO_NOFRAME
 , generateExposeEvents = ALLEGRO_GENERATE_EXPOSE_EVENTS
 }

-- ???: is there a way to do this that doesn't require this duplication?
allDisplayFlags :: [DisplayFlag]
allDisplayFlags = [windowed, fullscreen, fullscreenWindow, resizable, opengl, opengl3
                  , openglForwardCompatible, noframe, generateExposeEvents]

newtype DisplayOptionImportance = DisplayOptionImportance { unDisplayOptionImportance :: CInt }
#{enum DisplayOptionImportance, DisplayOptionImportance
 , require = ALLEGRO_REQUIRE
 , suggest = ALLEGRO_SUGGEST
 , dontCare = ALLEGRO_DONTCARE
 }

newtype DisplayOption = DisplayOption { unDisplayOption :: CInt }
#{enum DisplayOption, DisplayOption
 , colorSize = ALLEGRO_COLOR_SIZE
 , redSize = ALLEGRO_RED_SIZE
 , greenSize = ALLEGRO_GREEN_SIZE
 , blueSize = ALLEGRO_BLUE_SIZE
 , alphaSize = ALLEGRO_ALPHA_SIZE
 , redShift = ALLEGRO_RED_SHIFT
 , greenShift = ALLEGRO_GREEN_SHIFT
 , blueShift = ALLEGRO_BLUE_SHIFT
 , alphaShift = ALLEGRO_ALPHA_SHIFT
 , accRedSize = ALLEGRO_ACC_RED_SIZE
 , accGreenSize = ALLEGRO_ACC_GREEN_SIZE
 , accBlueSize = ALLEGRO_ACC_BLUE_SIZE
 , accAlphaSize = ALLEGRO_ACC_ALPHA_SIZE
 , stereo = ALLEGRO_STEREO
 , auxBuffers = ALLEGRO_AUX_BUFFERS
 , depthSize = ALLEGRO_DEPTH_SIZE
 , stencilSize = ALLEGRO_STENCIL_SIZE
 , sampleBuffers = ALLEGRO_SAMPLE_BUFFERS
 , samples = ALLEGRO_SAMPLES
 , renderMethod = ALLEGRO_RENDER_METHOD
 , floatColor = ALLEGRO_FLOAT_COLOR
 , floatDepth = ALLEGRO_FLOAT_DEPTH
 , singleBuffer = ALLEGRO_SINGLE_BUFFER
 , swapMethod = ALLEGRO_SWAP_METHOD
 , compatibleDisplay = ALLEGRO_COMPATIBLE_DISPLAY
 , canUpdateDisplayRegion = ALLEGRO_UPDATE_DISPLAY_REGION
 , vsync = ALLEGRO_VSYNC
 , maxBitmapSize = ALLEGRO_MAX_BITMAP_SIZE
 , supportNpotBitmap = ALLEGRO_SUPPORT_NPOT_BITMAP
 , canDrawIntoBitmap = ALLEGRO_CAN_DRAW_INTO_BITMAP
 , supportSeparateAlpha = ALLEGRO_SUPPORT_SEPARATE_ALPHA
 }

createDisplay :: Int -> Int -> IO (Display)
createDisplay w h = alCreateDisplay (toEnum w) (toEnum h)
foreign import ccall "allegro5/allegro.h al_create_display"
	alCreateDisplay :: CInt -> CInt -> IO (Display)

destroyDisplay :: Display -> IO ()
destroyDisplay = alDestroyDisplay
foreign import ccall "allegro5/allegro.h al_destroy_display"
	alDestroyDisplay :: Display -> IO ()

getNewDisplayFlags :: IO ([DisplayFlag])
getNewDisplayFlags = do
	flags <- alGetNewDisplayFlags
	return $ filter (\x -> toBool $ (.&.) flags $ unDisplayFlag x) allDisplayFlags
foreign import ccall "allegro5/allegro.h al_get_new_display_flags"
	alGetNewDisplayFlags :: IO (CInt)

getNewDisplayRefreshRate :: IO (Int)
getNewDisplayRefreshRate = liftM fromEnum alGetNewDisplayFlags
foreign import ccall "allegro5/allegro.h al_get_new_display_refresh_rate"
	alGetNewDisplayRefreshRate :: IO (CInt)

getNewWindowPosition :: IO ((Int,Int))
getNewWindowPosition = do
	alloca $ \px -> alloca $ \py -> do
		alGetNewWindowPosition px py
		x <- peek px :: IO (CInt); y <- peek py :: IO (CInt)
		return (fromEnum x, fromEnum y)
foreign import ccall "allegro5/allegro.h al_get_new_window_position"
	alGetNewWindowPosition :: Ptr (CInt) -> Ptr (CInt) -> IO ()

setNewDisplayOption :: DisplayOption -> Int -> DisplayOptionImportance -> IO ()
setNewDisplayOption option value importance =
	alSetNewDisplayOption option (toEnum value) importance
foreign import ccall "allegro5/allegro.h al_set_new_display_option"
	alSetNewDisplayOption :: DisplayOption -> CInt -> DisplayOptionImportance -> IO ()

getNewDisplayOption :: DisplayOption -> IO ((Int,DisplayOptionImportance))
getNewDisplayOption option = alloca $ \im -> do
	value <- alGetNewDisplayOption (unDisplayOption option) im
	importance <- peek im :: IO (CInt)
	return (fromEnum value,DisplayOptionImportance importance)
foreign import ccall "allegro5/allegro.h al_get_new_display_option"
	alGetNewDisplayOption :: CInt -> Ptr (CInt) -> IO (CInt)

resetNewDisplayOptions :: IO ()
resetNewDisplayOptions = alResetNewDisplayOptions
foreign import ccall "allegro5/allegro.h al_reset_new_display_options"
	alResetNewDisplayOptions :: IO ()

setNewDisplayFlags :: [DisplayFlag] -> IO ()
setNewDisplayFlags flags = alSetNewDisplayFlags flags'
	where flags' = combineFlags $ fmap unDisplayFlag flags
foreign import ccall "allegro5/allegro.h al_set_new_display_flags"
	alSetNewDisplayFlags :: CInt -> IO ()

setNewDisplayRefreshRate :: Int -> IO ()
setNewDisplayRefreshRate rate = alSetNewDisplayRefreshRate $ toEnum rate
foreign import ccall "allegro5/allegro.h al_set_new_display_refresh_rate"
	alSetNewDisplayRefreshRate :: CInt -> IO ()

setNewWindowPosition :: Int -> Int -> IO ()
setNewWindowPosition x y = alSetNewWindowPosition (toEnum x) (toEnum y)
foreign import ccall "allegro5/allegro.h al_set_new_window_position"
	alSetNewWindowPosition :: CInt -> CInt -> IO ()

acknowledgeResize :: Display -> IO (Bool)
acknowledgeResize d = liftM toBool $ alAcknowledgeResize d
foreign import ccall "allegro5/allegro.h al_acknowledge_resize"
	alAcknowledgeResize :: Display -> IO (CInt)

flipDisplay :: IO ()
flipDisplay = alFlipDisplay
foreign import ccall "allegro5/allegro.h al_flip_display"
	alFlipDisplay :: IO ()

getBackbuffer :: Display -> IO (Bitmap)
getBackbuffer = alGetBackbuffer
foreign import ccall "allegro5/allegro.h al_get_backbuffer"
	alGetBackbuffer :: Display -> IO (Bitmap)

getDisplayFlags :: Display -> IO ([DisplayFlag])
getDisplayFlags d = do
	flags <- alGetDisplayFlags d
	return $ filter (\x -> toBool $ (.&.) (unDisplayFlag x) flags) allDisplayFlags
foreign import ccall "allegro5/allegro.h al_get_display_flags"
	alGetDisplayFlags :: Display -> IO (CInt)

getDisplayHeight :: Display -> IO (Int)
getDisplayHeight d = liftM fromEnum $ alGetDisplayHeight d
foreign import ccall "allegro5/allegro.h al_get_display_height"
	alGetDisplayHeight :: Display -> IO (CInt)

getDisplayRefreshRate :: Display -> IO (Int)
getDisplayRefreshRate d = liftM fromEnum $ alGetDisplayRefreshRate d
foreign import ccall "allegro5/allegro.h al_get_display_refresh_rate"
	alGetDisplayRefreshRate :: Display -> IO (CInt)

getDisplayWidth :: Display -> IO (Int)
getDisplayWidth d = liftM fromEnum $ alGetDisplayWidth d
foreign import ccall "allegro5/allegro.h al_get_display_width"
	alGetDisplayWidth :: Display -> IO (CInt)

getWindowPosition :: Display -> IO ((Int,Int))
getWindowPosition d = alloca $ \px -> alloca $ \py -> do
	alGetWindowPosition d px py
	x <- peek px :: IO (CInt); y <- peek py :: IO (CInt)
	return (fromEnum x, fromEnum y)
foreign import ccall "allegro5/allegro.h al_get_window_position"
	alGetWindowPosition :: Display -> Ptr (CInt) -> Ptr (CInt) -> IO ()

inhibitScreensaver :: Bool -> IO (Bool)
inhibitScreensaver inhibit = liftM toBool $ alInhibitScreensaver inhibit'
	where inhibit' = fromBool inhibit :: CInt
foreign import ccall "allegro5/allegro.h al_inhibit_screensaver"
	alInhibitScreensaver :: CInt -> IO (CInt)

resizeDisplay :: Display -> Int -> Int -> IO (Bool)
resizeDisplay d w h = liftM toBool $ alResizeDisplay d (toEnum w) (toEnum h)
foreign import ccall "allegro5/allegro.h al_resize_display"
	alResizeDisplay :: Display -> CInt -> CInt -> IO (CInt)

setDisplayIcon :: Display -> Bitmap -> IO ()
setDisplayIcon = alSetDisplayIcon
foreign import ccall "allegro5/allegro.h al_set_display_icon"
	alSetDisplayIcon :: Display -> Bitmap -> IO ()

getDisplayOption :: Display -> DisplayOption -> IO (Int)
getDisplayOption d option = liftM fromEnum $ alGetDisplayOption d option
foreign import ccall "allegro5/allegro.h al_get_display_option"
	alGetDisplayOption :: Display -> DisplayOption -> IO (CInt)

setWindowPosition :: Display -> Int -> Int -> IO ()
setWindowPosition d x y = alSetWindowPosition d (toEnum x) (toEnum y)
foreign import ccall "allegro5/allegro.h al_set_window_position"
	alSetWindowPosition :: Display -> CInt -> CInt -> IO ()

setWindowTitle :: Display -> String -> IO ()
setWindowTitle d title = withCString title $ alSetWindowTitle d
foreign import ccall "allegro5/allegro.h al_set_window_title"
	alSetWindowTitle :: Display -> CString -> IO ()

toggleDisplayFlag :: Display -> DisplayFlag -> Bool -> IO (Bool)
toggleDisplayFlag d flag onoff = liftM toBool $ alToggleDisplayFlag d flag onoff'
	where onoff' = fromBool onoff :: CInt
foreign import ccall "allegro5/allegro.h al_toggle_display_flag"
	alToggleDisplayFlag :: Display -> DisplayFlag -> CInt -> IO (CInt)

updateDisplayRegion :: Int -> Int -> Int -> Int -> IO ()
updateDisplayRegion x y w h =
	alUpdateDisplayRegion (toEnum x) (toEnum y) (toEnum w) (toEnum h)
foreign import ccall "allegro5/allegro.h al_update_display_region"
	alUpdateDisplayRegion :: CInt -> CInt -> CInt -> CInt -> IO ()

waitForVSync :: IO (Bool)
waitForVSync = liftM toBool alWaitForVSync
foreign import ccall "allegro5/allegro.h al_wait_for_vsync"
	alWaitForVSync :: IO (CInt)

