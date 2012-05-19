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
import Allegro.Config
import Allegro.Path

foreign import ccall "c/al-wrapper.h wal_init" alInit :: IO (CInt)
-- foreign import ccall "allegro5/allegro.h al_install_system" alInstallSystem :: CInt -> Ptr CInt -> IO (CInt)
foreign import ccall "allegro5/allegro.h al_uninstall_system" alUninstallSystem :: IO ()
foreign import ccall "allegro5/allegro.h al_is_system_installed" alIsSystemInstalled :: IO (CInt)
foreign import ccall "allegro5/allegro.h al_get_allegro_version" alGetAllegroVersion :: IO (CUInt)
foreign import ccall "allegro5/allegro.h al_get_standard_path" alGetStandardPath :: CInt -> IO (Path)
foreign import ccall "allegro5/allegro.h al_set_exe_name" alSetExeName :: CString -> IO ()
foreign import ccall "allegro5/allegro.h al_set_app_name" alSetAppName :: CString -> IO ()
foreign import ccall "allegro5/allegro.h al_set_org_name" alSetOrgName :: CString -> IO ()
foreign import ccall "allegro5/allegro.h al_get_app_name" alGetAppName :: IO (CString)
foreign import ccall "allegro5/allegro.h al_get_org_name" alGetOrgName :: IO (CString)
foreign import ccall "allegro5/allegro.h al_get_system_config" alGetSystemConfig :: IO (Config)

initialize :: IO (Bool)
initialize = liftM (/= 0) alInit

-- TODO: need to figure out how to pass in a function...
installSystem :: Int -> (Int -> IO ()) -> IO ()
installSystem version atExit = undefined

uninstallSystem :: IO ()
uninstallSystem = alUninstallSystem

isSystemInstalled :: IO (Bool)
isSystemInstalled = liftM (/= 0) alIsSystemInstalled

getAllegroVersion :: IO (Integer)
getAllegroVersion = liftM toInteger alGetAllegroVersion

getStandardPath :: Int -> IO (Path)
getStandardPath id = alGetStandardPath (toEnum id)

-- TODO: make sure this string doesn't get randomly destroyed
setExeName :: String -> IO ()
setExeName name = withCString name alSetExeName

-- TODO: make sure this string doesn't get randomly destroyed
setAppName :: String -> IO ()
setAppName name = withCString name alSetAppName

-- TODO: make sure this string doesn't get randomly destroyed
setOrgName :: String -> IO ()
setOrgName name = withCString name alSetOrgName

getAppName :: IO (String)
getAppName = alGetAppName >>= peekCString

getOrgName :: IO (String)
getOrgName = alGetOrgName >>= peekCString

getSystemConfig :: IO (Config)
getSystemConfig = alGetSystemConfig
