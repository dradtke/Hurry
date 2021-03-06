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

{-# LANGUAGE DeriveDataTypeable #-}

module Allegro.Mouse
( Mouse
, installMouse
, isMouseInstalled
, uninstallMouse
, getMouseNumAxes
, getMouseNumButtons
, getMouseAxis
, getMousePosition
, getMouseZ
, getMouseW
, setMousePosition
, setMouseZ
, setMouseW
, setMouseAxis
) where

#include <allegro5/allegro.h>

import Allegro.Display

import Control.Monad
import Data.Bits
import Data.Data
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data MouseStruct = MouseStruct { mouseX :: Int
                               , mouseY :: Int
                               , mouseW :: Int
                               , mouseZ :: Int
                               , mouseButtons :: [MouseButton]
                               } deriving (Typeable)
type Mouse = Ptr (MouseStruct)

newtype MouseButton = MouseButton { unMouseButton :: Int } deriving (Eq, Show, Typeable, Data)
#{enum MouseButton, MouseButton
 , mouseLeft = 1
 , mouseRight = 2
 , mouseMiddle = 4
 }

allMouseButtons :: [MouseButton]
allMouseButtons = [mouseLeft, mouseRight, mouseMiddle]

instance Storable MouseStruct where
	sizeOf _ = #{size ALLEGRO_MOUSE_STATE}
	alignment _ = alignment (undefined :: Int)
	peek p = do mouseX <- liftM fromEnum (#{peek ALLEGRO_MOUSE_STATE, x} p :: IO (CInt))
                    mouseY <- liftM fromEnum (#{peek ALLEGRO_MOUSE_STATE, y} p :: IO (CInt))
                    mouseW <- liftM fromEnum (#{peek ALLEGRO_MOUSE_STATE, w} p :: IO (CInt))
                    mouseZ <- liftM fromEnum (#{peek ALLEGRO_MOUSE_STATE, z} p :: IO (CInt))
                    btns <- liftM fromEnum (#{peek ALLEGRO_MOUSE_STATE, buttons} p :: IO (CInt))
                    let mouseButtons = filter (\x -> toBool $ (.&.) btns $ unMouseButton x) allMouseButtons
                    return MouseStruct {..}

installMouse :: IO (Bool)
installMouse = liftM toBool alInstallMouse
foreign import ccall "allegro5/allegro.h al_install_mouse"
	alInstallMouse :: IO (CInt)

isMouseInstalled :: IO (Bool)
isMouseInstalled = liftM toBool alIsMouseInstalled
foreign import ccall "allegro5/allegro.h al_is_mouse_installed"
	alIsMouseInstalled :: IO (CInt)

uninstallMouse :: IO ()
uninstallMouse = alUninstallMouse
foreign import ccall "allegro5/allegro.h al_uninstall_mouse"
	alUninstallMouse :: IO ()

getMouseNumAxes :: IO (Int)
getMouseNumAxes = liftM fromEnum alGetMouseNumAxes
foreign import ccall "allegro5/allegro.h al_get_mouse_num_axes"
	alGetMouseNumAxes :: IO (CInt)

getMouseNumButtons :: IO (Int)
getMouseNumButtons = liftM fromEnum alGetMouseNumButtons
foreign import ccall "allegro5/allegro.h al_get_mouse_num_buttons"
	alGetMouseNumButtons :: IO (CInt)

-- No need to expose this, it's only used internally.
foreign import ccall "allegro5/allegro.h al_get_mouse_state"
	alGetMouseState :: Mouse -> IO ()

-- TODO: create a data type for the different axes
getMouseAxis :: Int -> IO (Int)
getMouseAxis axis = alloca $ \p -> do
	alGetMouseState p
	liftM fromEnum $ alGetMouseStateAxis p (toEnum axis)
foreign import ccall "allegro5/allegro.h al_get_mouse_state_axis"
	alGetMouseStateAxis :: Mouse -> CInt -> IO (CInt)

getMousePosition :: IO ((Int,Int))
getMousePosition = alloca $ \p -> do
	alGetMouseState p
	m <- peek p
	return (mouseX m, mouseY m)

getMouseZ :: IO (Int)
getMouseZ = alloca $ \p -> do
	alGetMouseState p
	m <- peek p
	return $ mouseZ m

getMouseW :: IO (Int)
getMouseW = alloca $ \p -> do
	alGetMouseState p
	m <- peek p
	return $ mouseW m

isMouseButtonDown :: MouseButton -> IO (Bool)
isMouseButtonDown btn = alloca $ \p -> do
	alGetMouseState p
	liftM toBool $ alMouseButtonDown p $ toEnum $ unMouseButton btn
foreign import ccall "allegro5/allegro.h al_mouse_button_down"
	alMouseButtonDown :: Mouse -> CInt -> IO (CInt)

setMousePosition :: Display -> (Int,Int) -> IO (Bool)
setMousePosition d (x,y) = liftM toBool $ alSetMouseXY d (toEnum x) (toEnum y)
foreign import ccall "allegro5/allegro.h al_set_mouse_xy"
	alSetMouseXY :: Display -> CInt -> CInt -> IO (CInt)

setMouseZ :: Int -> IO (Bool)
setMouseZ z = liftM toBool $ alSetMouseZ (toEnum z)
foreign import ccall "allegro5/allegro.h al_set_mouse_z"
	alSetMouseZ :: CInt -> IO (CInt)

setMouseW :: Int -> IO (Bool)
setMouseW w = liftM toBool $ alSetMouseW (toEnum w)
foreign import ccall "allegro5/allegro.h al_set_mouse_w"
	alSetMouseW :: CInt -> IO (CInt)

-- TODO: is there a datatype for axes yet?
setMouseAxis :: Int -> Int -> IO (Bool)
setMouseAxis axis value = liftM toBool $ alSetMouseAxis (toEnum axis) (toEnum value)
foreign import ccall "allegro5/allegro.h al_set_mouse_axis"
	alSetMouseAxis :: CInt -> CInt -> IO (CInt)

-- TODO: mouse cursors
