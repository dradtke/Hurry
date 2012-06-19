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

module Allegro.Keyboard
( Keyboard
, KeyboardStruct
, Key(..)
, installKeyboard
, isKeyboardInstalled
, uninstallKeyboard
, isKeyDown
, keycodeToName
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

data KeyboardStruct deriving (Typeable)
type Keyboard = Ptr (KeyboardStruct)

newtype Key = Key { unKey :: Int } deriving (Eq, Show, Read, Typeable, Data)
#{enum Key, Key
 , keyA = ALLEGRO_KEY_A
 , keyB = ALLEGRO_KEY_B
 , keyC = ALLEGRO_KEY_C
 , keyD = ALLEGRO_KEY_D
 , keyE = ALLEGRO_KEY_E
 , keyF = ALLEGRO_KEY_F
 , keyG = ALLEGRO_KEY_G
 , keyH = ALLEGRO_KEY_H
 , keyI = ALLEGRO_KEY_I
 , keyJ = ALLEGRO_KEY_J
 , keyK = ALLEGRO_KEY_K
 , keyL = ALLEGRO_KEY_L
 , keyM = ALLEGRO_KEY_M
 , keyN = ALLEGRO_KEY_N
 , keyO = ALLEGRO_KEY_O
 , keyP = ALLEGRO_KEY_P
 , keyQ = ALLEGRO_KEY_Q
 , keyR = ALLEGRO_KEY_R
 , keyS = ALLEGRO_KEY_S
 , keyT = ALLEGRO_KEY_T
 , keyU = ALLEGRO_KEY_U
 , keyV = ALLEGRO_KEY_V
 , keyW = ALLEGRO_KEY_W
 , keyX = ALLEGRO_KEY_X
 , keyY = ALLEGRO_KEY_Y
 , keyZ = ALLEGRO_KEY_Z
 , key0 = ALLEGRO_KEY_0
 , key1 = ALLEGRO_KEY_1
 , key2 = ALLEGRO_KEY_2
 , key3 = ALLEGRO_KEY_3
 , key4 = ALLEGRO_KEY_4
 , key5 = ALLEGRO_KEY_5
 , key6 = ALLEGRO_KEY_6
 , key7 = ALLEGRO_KEY_7
 , key8 = ALLEGRO_KEY_8
 , key9 = ALLEGRO_KEY_9
 , keyF1 = ALLEGRO_KEY_F1
 , keyF2 = ALLEGRO_KEY_F2
 , keyF3 = ALLEGRO_KEY_F3
 , keyF4 = ALLEGRO_KEY_F4
 , keyF5 = ALLEGRO_KEY_F5
 , keyF6 = ALLEGRO_KEY_F6
 , keyF7 = ALLEGRO_KEY_F7
 , keyF8 = ALLEGRO_KEY_F8
 , keyF9 = ALLEGRO_KEY_F9
 , keyF10 = ALLEGRO_KEY_F10
 , keyF11 = ALLEGRO_KEY_F11
 , keyF12 = ALLEGRO_KEY_F12
 , keyEscape = ALLEGRO_KEY_ESCAPE
 , keyTilde = ALLEGRO_KEY_TILDE
 , keyMinus = ALLEGRO_KEY_MINUS
 , keyEquals = ALLEGRO_KEY_EQUALS
 , keyBackspace = ALLEGRO_KEY_BACKSPACE
 , keyTab = ALLEGRO_KEY_TAB
 , keyOpenBrace = ALLEGRO_KEY_OPENBRACE
 , keyCloseBrace = ALLEGRO_KEY_CLOSEBRACE
 , keyEnter = ALLEGRO_KEY_ENTER
 , keySemicolon = ALLEGRO_KEY_SEMICOLON
 , keyQuote = ALLEGRO_KEY_QUOTE
 , keyBackslash = ALLEGRO_KEY_BACKSLASH
 , keyBackslash2 = ALLEGRO_KEY_BACKSLASH2
 , keyComma = ALLEGRO_KEY_COMMA
 , keyFullStop = ALLEGRO_KEY_FULLSTOP
 , keySlash = ALLEGRO_KEY_SLASH
 , keySpace = ALLEGRO_KEY_SPACE
 , keyInsert = ALLEGRO_KEY_INSERT
 , keyDelete = ALLEGRO_KEY_DELETE
 , keyHome = ALLEGRO_KEY_HOME
 , keyEnd = ALLEGRO_KEY_END
 , keyPageUp = ALLEGRO_KEY_PGUP
 , keyPageDown = ALLEGRO_KEY_PGDN
 , keyLeft = ALLEGRO_KEY_LEFT
 , keyRight = ALLEGRO_KEY_RIGHT
 , keyUp = ALLEGRO_KEY_UP
 , keyDown = ALLEGRO_KEY_DOWN
 , keyPadSlash = ALLEGRO_KEY_PAD_SLASH
 , keyPadAsterisk = ALLEGRO_KEY_PAD_ASTERISK
 , keyPadMinus = ALLEGRO_KEY_PAD_MINUS
 , keyPadPlus = ALLEGRO_KEY_PAD_PLUS
 , keyPadDelete = ALLEGRO_KEY_PAD_DELETE
 , keyPadEnter = ALLEGRO_KEY_PAD_ENTER
 , keyPrintscreen = ALLEGRO_KEY_PRINTSCREEN
 , keyPause = ALLEGRO_KEY_PAUSE
 , keyAbntC1 = ALLEGRO_KEY_ABNT_C1
 , keyYen = ALLEGRO_KEY_YEN
 , keyKana = ALLEGRO_KEY_KANA
 , keyConvert = ALLEGRO_KEY_CONVERT
 , keyNoConvert = ALLEGRO_KEY_NOCONVERT
 , keyAt = ALLEGRO_KEY_AT
 , keyCircumflex = ALLEGRO_KEY_CIRCUMFLEX
 , keyColon2 = ALLEGRO_KEY_COLON2
 , keyKanji = ALLEGRO_KEY_KANJI
 , keyLShift = ALLEGRO_KEY_LSHIFT
 , keyRShift = ALLEGRO_KEY_RSHIFT
 , keyLCtrl = ALLEGRO_KEY_LCTRL
 , keyRCtrl = ALLEGRO_KEY_RCTRL
 , keyAlt = ALLEGRO_KEY_ALT
 , keyAltgr = ALLEGRO_KEY_ALTGR
 , keyLWin = ALLEGRO_KEY_LWIN
 , keyRWin = ALLEGRO_KEY_RWIN
 , keyMenu = ALLEGRO_KEY_MENU
 , keyScrollLock = ALLEGRO_KEY_SCROLLLOCK
 , keyNumLock = ALLEGRO_KEY_NUMLOCK
 , keyCapsLock = ALLEGRO_KEY_CAPSLOCK
 , keyPadEquals = ALLEGRO_KEY_PAD_EQUALS
 , keyBackquote = ALLEGRO_KEY_BACKQUOTE
 , keySemicolon2 = ALLEGRO_KEY_SEMICOLON2
 , keyCommand = ALLEGRO_KEY_COMMAND
 }

instance Storable KeyboardStruct where
	sizeOf _ = #{size ALLEGRO_KEYBOARD_STATE}
	alignment _ = alignment (undefined :: Int)

installKeyboard :: IO (Bool)
installKeyboard = liftM toBool alInstallKeyboard
foreign import ccall "allegro5/allegro.h al_install_keyboard"
	alInstallKeyboard :: IO (CInt)

isKeyboardInstalled :: IO (Bool)
isKeyboardInstalled = liftM toBool alIsKeyboardInstalled
foreign import ccall "allegro5/allegro.h al_is_keyboard_installed"
	alIsKeyboardInstalled :: IO (CInt)

uninstallKeyboard :: IO (Bool)
uninstallKeyboard = liftM toBool alUninstallKeyboard
foreign import ccall "allegro5/allegro.h al_uninstall_keyboard"
	alUninstallKeyboard :: IO (CInt)

-- No need to expose this, it's only used internally.
foreign import ccall "allegro5/allegro.h al_get_keyboard_state"
	alGetKeyboardState :: Keyboard -> IO ()

isKeyDown :: Key -> IO (Bool)
isKeyDown key = alloca $ \p -> do
	alGetKeyboardState p
	liftM toBool $ alKeyDown p (toEnum $ unKey key)
foreign import ccall "allegro5/allegro.h al_key_down"
	alKeyDown :: Keyboard -> CInt -> IO (CInt)

keycodeToName :: Key -> IO (String)
keycodeToName key = alKeycodeToName (toEnum $ unKey key) >>= peekCString
foreign import ccall "allegro5/allegro.h al_keycode_to_name"
	alKeycodeToName :: CInt -> IO (CString)

-- TODO: set_keyboard_leds()
