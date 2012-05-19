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

module Allegro.Event
( EventSource
, UserEventDescriptor
, Event(..)
) where

#include <allegro5/allegro.h>

import Allegro.Keyboard
import Allegro.Mouse
import Allegro.Display
import Allegro.Joystick
import Allegro.Timer

import Foreign.Ptr
import Foreign.Storable
import Data.Word

data EventSourceStruct
type EventSource = Ptr (EventSourceStruct)

data UserEventDescriptorStruct
type UserEventDescriptor = Ptr (UserEventDescriptorStruct)

data Event = AnyEvent { eventType :: Word, eventSource :: EventSource, eventTimestamp :: Double }
           | JoystickAxisEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double
                               , eventJoystickId :: Joystick, eventJoystickAxis :: Int
                               , eventJoystickPos :: Float }
           | JoystickButtonDownEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double
                                     , eventJoystickId :: Joystick, eventJoystickButton :: Int }
           | JoystickButtonUpEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double
                                   , eventJoystickId :: Joystick, eventJoystickButton :: Int }
           | JoystickConfigurationEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double }
           | KeyDownEvent { eventKeyboardSource :: Keyboard, eventTimestamp :: Double
                          , eventKeycode :: Key, eventDisplay :: Display }
           | KeyUpEvent { eventKeyboardSource :: Keyboard, eventTimestamp :: Double
                        , eventKeycode :: Key, eventDisplay :: Display }
           | KeyCharEvent { eventKeyboardSource :: Keyboard, eventTimestamp :: Double
                          , eventKeycode :: Key, eventUnichar :: Char, eventModifiers :: Word
                          , eventRepeat :: Bool, eventDisplay :: Display }
           | MouseAxesEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                            , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                            , eventDX :: Int, eventDY :: Int, eventDZ :: Int, eventDW :: Int
                            , eventDisplay :: Display }
           | MouseDownEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                            , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                            , eventButton :: Int, eventDisplay :: Display }
           | MouseUpEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                          , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                          , eventButton :: Int, eventDisplay :: Display }
           | MouseEnterDisplayEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                                    , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                                    , eventDisplay :: Display }
           | MouseLeaveDisplayEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                                    , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                                    , eventDisplay :: Display }
           | MouseWarpedEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                              , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                              , eventDX :: Int, eventDY :: Int, eventDZ :: Int, eventDW :: Int
                              , eventDisplay :: Display }
           | TimerEvent { eventTimerSource :: Timer, eventTimestamp :: Double
                        , eventTimerCount :: Word64 }
           | DisplayExposeEvent { eventDisplaySource :: Display, eventTimestamp :: Double
                                , eventX :: Int, eventY :: Int, eventDisplayWidth :: Int
                                , eventDisplayHeight :: Int }
           | DisplayResizeEvent { eventDisplaySource :: Display, eventTimestamp :: Double
                                , eventX :: Int, eventY :: Int, eventDisplayWidth :: Int
                                , eventDisplayHeight :: Int }
           | DisplayCloseEvent { eventDisplaySource :: Display, eventTimestamp :: Double }
           | DisplayLostEvent { eventDisplaySource :: Display, eventTimestamp :: Double }
           | DisplayFoundEvent { eventDisplaySource :: Display, eventTimestamp :: Double }
           | DisplaySwitchInEvent { eventDisplaySource :: Display, eventTimestamp :: Double }
           | DisplaySwitchOutEvent { eventDisplaySource :: Display, eventTimestamp :: Double }
           | DisplayOrientationEvent { eventDisplaySource :: Display, eventTimestamp :: Double
                                     , eventDisplayOrientation :: Int }
           | UserEvent { eventType :: Word, eventSource :: EventSource
                      , eventTimestamp :: Double, eventData1 :: Ptr (), eventData2 :: Ptr ()
                      , eventData3 :: Ptr (), eventData4 :: Ptr () }

instance Storable Event where
	sizeOf _ = #{size ALLEGRO_EVENT}
	alignment _ = alignment (undefined :: Int)
	peek p = do
		typ <- #{peek ALLEGRO_EVENT, any.type} p :: IO (Word)
		parseEvent typ p
	poke _ _ = undefined

parseEvent :: Word -> Ptr Event -> IO (Event)
parseEvent typ p
	| typ > 0   && typ < 10 = parseJoystickEvent typ p
	| typ >= 10 && typ < 20 = parseKeyboardEvent typ p
	| typ >= 20 && typ < 30 = parseMouseEvent typ p
	| typ >= 30 && typ < 40 = parseTimerEvent typ p
	| typ >= 40 && typ < 50 = parseDisplayEvent typ p
	| typ >= 512		= parseUserEvent typ p
	| otherwise		= error $ "Unknown builtin event type: " ++ show typ

parseJoystickEvent :: Word -> Ptr Event -> IO (Event)
parseJoystickEvent typ p = do
	eventJoystickSource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Joystick)
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventJoystickId <- #{peek ALLEGRO_EVENT, joystick.id} p :: IO (Joystick)
	eventJoystickAxis <- #{peek ALLEGRO_EVENT, joystick.axis} p :: IO (Int)
	eventJoystickPos <- #{peek ALLEGRO_EVENT, joystick.pos} p :: IO (Float)
	eventJoystickButton <- #{peek ALLEGRO_EVENT, joystick.button} p :: IO (Int)
	return $ case typ of
		1 -> JoystickAxisEvent {..}
		2 -> JoystickButtonDownEvent {..}
		3 -> JoystickButtonUpEvent {..}
		4 -> JoystickConfigurationEvent {..}
		_ -> error $ "Unknown joystick event type: " ++ show typ

parseKeyboardEvent :: Word -> Ptr Event -> IO (Event)
parseKeyboardEvent typ p = do
	eventKeyboardSource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Keyboard)
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventDisplay <- #{peek ALLEGRO_EVENT, keyboard.display} p :: IO (Display)
	eventKeycode <- #{peek ALLEGRO_EVENT, keyboard.keycode} p :: IO (Key)
	eventUnichar <- #{peek ALLEGRO_EVENT, keyboard.unichar} p :: IO (Char)
	eventModifiers <- #{peek ALLEGRO_EVENT, keyboard.modifiers} p :: IO (Word)
	eventRepeat <- #{peek ALLEGRO_EVENT, keyboard.repeat} p :: IO (Bool)
	return $ case typ of
		10 -> KeyDownEvent {..}
		11 -> KeyCharEvent {..}
		12 -> KeyUpEvent {..}
		_  -> error $ "Unknown keyboard event: " ++ show typ

parseMouseEvent :: Word -> Ptr Event -> IO (Event)
parseMouseEvent typ p = do
	eventMouseSource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Mouse)
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventX <- #{peek ALLEGRO_EVENT, mouse.x} p :: IO (Int)
	eventY <- #{peek ALLEGRO_EVENT, mouse.y} p :: IO (Int)
	eventZ <- #{peek ALLEGRO_EVENT, mouse.z} p :: IO (Int)
	eventW <- #{peek ALLEGRO_EVENT, mouse.w} p :: IO (Int)
	eventDX <- #{peek ALLEGRO_EVENT, mouse.dx} p :: IO (Int)
	eventDY <- #{peek ALLEGRO_EVENT, mouse.dy} p :: IO (Int)
	eventDZ <- #{peek ALLEGRO_EVENT, mouse.dz} p :: IO (Int)
	eventDW <- #{peek ALLEGRO_EVENT, mouse.dw} p :: IO (Int)
	eventButton <- #{peek ALLEGRO_EVENT, mouse.button} p :: IO (Int)
	eventDisplay <- #{peek ALLEGRO_EVENT, mouse.display} p :: IO (Display)
	return $ case typ of
		20 -> MouseAxesEvent {..}
		21 -> MouseDownEvent {..}
		22 -> MouseUpEvent {..}
		23 -> MouseEnterDisplayEvent {..}
		24 -> MouseLeaveDisplayEvent {..}
		25 -> MouseWarpedEvent {..}
		_  -> error $ "Unknown mouse event: " ++ show typ

parseTimerEvent :: Word -> Ptr Event -> IO (Event)
parseTimerEvent typ p = do
	eventTimerSource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Timer)
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventTimerCount <- #{peek ALLEGRO_EVENT, timer.count} p :: IO (Word64)
	return $ case typ of
		30 -> TimerEvent {..}
		_  -> error $ "Unknown timer event: " ++ show typ

parseDisplayEvent :: Word -> Ptr Event -> IO (Event)
parseDisplayEvent typ p = do
	eventDisplaySource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Display)
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventX <- #{peek ALLEGRO_EVENT, display.x} p :: IO (Int)
	eventY <- #{peek ALLEGRO_EVENT, display.y} p :: IO (Int)
	eventDisplayWidth <- #{peek ALLEGRO_EVENT, display.width} p :: IO (Int)
	eventDisplayHeight <- #{peek ALLEGRO_EVENT, display.height} p :: IO (Int)
	eventDisplayOrientation <- #{peek ALLEGRO_EVENT, display.orientation} p :: IO (Int)
	return $ case typ of
		40 -> DisplayExposeEvent {..}
		41 -> DisplayResizeEvent {..}
		42 -> DisplayCloseEvent {..}
		43 -> DisplayLostEvent {..}
		44 -> DisplayFoundEvent {..}
		45 -> DisplaySwitchInEvent {..}
		46 -> DisplaySwitchOutEvent {..}
		47 -> DisplayOrientationEvent {..}
		_  -> error $ "Unknown display event: " ++ show typ

parseUserEvent :: Word -> Ptr Event -> IO (Event)
parseUserEvent typ p = do
	eventSource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (EventSource)
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventData1 <- #{peek ALLEGRO_EVENT, user.data1} p :: IO (Ptr ())
	eventData2 <- #{peek ALLEGRO_EVENT, user.data2} p :: IO (Ptr ())
	eventData3 <- #{peek ALLEGRO_EVENT, user.data3} p :: IO (Ptr ())
	eventData4 <- #{peek ALLEGRO_EVENT, user.data4} p :: IO (Ptr ())
	let eventType = typ
	return UserEvent {..}
