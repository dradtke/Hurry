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
) where

#include <allegro5/allegro.h>

import Control.Monad
import Data.Data
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data MouseStruct deriving (Typeable)
type Mouse = Ptr (MouseStruct)

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
