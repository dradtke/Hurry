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

module Allegro.System
( initialize
, installSystem
, uninstallSystem
, isSystemInstalled
, getAllegroVersion
, getStandardPath
, setExeName
, setAppName
, setOrgName
, getAppName
, getOrgName
, getSystemConfig
) where

import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Allegro.Config
import Allegro.Path

initialize :: IO (Bool)
initialize = liftM toBool alInit
foreign import ccall "c/al-wrapper.h wal_init"
	alInit :: IO (CInt)

-- TODO: need to figure out how to pass in a function pointer...
installSystem :: Int -> (Int -> IO ()) -> IO ()
installSystem version atExit = undefined
-- foreign import ccall "allegro5/allegro.h al_install_system"
-- 	alInstallSystem :: CInt -> Ptr CInt -> IO (CInt)

uninstallSystem :: IO ()
uninstallSystem = alUninstallSystem
foreign import ccall "allegro5/allegro.h al_uninstall_system"
	alUninstallSystem :: IO ()

isSystemInstalled :: IO (Bool)
isSystemInstalled = liftM toBool alIsSystemInstalled
foreign import ccall "allegro5/allegro.h al_is_system_installed"
	alIsSystemInstalled :: IO (CInt)

getAllegroVersion :: IO (Integer)
getAllegroVersion = liftM toInteger alGetAllegroVersion
foreign import ccall "allegro5/allegro.h al_get_allegro_version"
	alGetAllegroVersion :: IO (CUInt)

getStandardPath :: Int -> IO (Path)
getStandardPath id = alGetStandardPath $ toEnum id
foreign import ccall "allegro5/allegro.h al_get_standard_path"
	alGetStandardPath :: CInt -> IO (Path)

-- TODO: make sure this string doesn't get randomly destroyed
setExeName :: String -> IO ()
setExeName name = withCString name alSetExeName
foreign import ccall "allegro5/allegro.h al_set_exe_name"
	alSetExeName :: CString -> IO ()

-- TODO: make sure this string doesn't get randomly destroyed
setAppName :: String -> IO ()
setAppName name = withCString name alSetAppName
foreign import ccall "allegro5/allegro.h al_set_app_name"
	alSetAppName :: CString -> IO ()

-- TODO: make sure this string doesn't get randomly destroyed
setOrgName :: String -> IO ()
setOrgName name = withCString name alSetOrgName
foreign import ccall "allegro5/allegro.h al_set_org_name"
	alSetOrgName :: CString -> IO ()

getAppName :: IO (String)
getAppName = alGetAppName >>= peekCString
foreign import ccall "allegro5/allegro.h al_get_app_name"
	alGetAppName :: IO (CString)

getOrgName :: IO (String)
getOrgName = alGetOrgName >>= peekCString
foreign import ccall "allegro5/allegro.h al_get_org_name"
	alGetOrgName :: IO (CString)

getSystemConfig :: IO (Config)
getSystemConfig = alGetSystemConfig
foreign import ccall "allegro5/allegro.h al_get_system_config"
	alGetSystemConfig :: IO (Config)
