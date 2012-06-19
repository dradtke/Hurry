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

module Allegro.Event
( EventSource
, UserEventDescriptor
, EventQueue
, Event(..)
, createEventQueue
, destroyEventQueue
, registerEventSource
, unregisterEventSource
, isEventQueueEmpty
, getNextEvent
, peekNextEvent
, dropNextEvent
, flushEventQueue
, waitForEvent
--, waitForEventTimed
, initUserEventSource
, destroyUserEventSource
, getDisplayEventSource
, getKeyboardEventSource
) where

#include <allegro5/allegro.h>

import Allegro.Keyboard
import Allegro.Mouse
import Allegro.Display
import Allegro.Joystick
import Allegro.Timer

import Control.Monad
import Data.Data
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Data.Word

data EventSourceStruct deriving (Typeable)
type EventSource = Ptr (EventSourceStruct)

data UserEventDescriptorStruct deriving (Typeable)
type UserEventDescriptor = Ptr (UserEventDescriptorStruct)

data EventQueueStruct
type EventQueue = ForeignPtr (EventQueueStruct)

-- ???: Do the pointer references here create memory leaks?
type EventPtr = Ptr (Event)
data Event = AnyEvent { eventType :: Word, eventSource :: EventSource, eventTimestamp :: Double }
           | JoystickAxisEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double
                               , eventJoystickId :: Joystick, eventJoystickAxis :: Int
                               , eventJoystickPos :: Float }
           | JoystickButtonDownEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double
                                     , eventJoystickId :: Joystick, eventJoystickButton :: Int }
           | JoystickButtonUpEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double
                                   , eventJoystickId :: Joystick, eventJoystickButton :: Int }
           | JoystickConfigurationEvent { eventJoystickSource :: Joystick, eventTimestamp :: Double }
           | KeyDownEvent { eventKeyboardSource :: Ptr (KeyboardStruct), eventTimestamp :: Double
                          , eventKeycode :: Key, eventDisplay :: Ptr (DisplayStruct) }
           | KeyUpEvent { eventKeyboardSource :: Ptr (KeyboardStruct), eventTimestamp :: Double
                        , eventKeycode :: Key, eventDisplay :: Ptr (DisplayStruct) }
           | KeyCharEvent { eventKeyboardSource :: Ptr (KeyboardStruct), eventTimestamp :: Double
                          , eventKeycode :: Key, eventUnichar :: Char, eventModifiers :: Word
                          , eventRepeat :: Bool, eventDisplay :: Ptr (DisplayStruct) }
           | MouseAxesEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                            , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                            , eventDX :: Int, eventDY :: Int, eventDZ :: Int, eventDW :: Int
                            , eventDisplay :: Ptr (DisplayStruct) }
           | MouseDownEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                            , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                            , eventButton :: Int, eventDisplay :: Ptr (DisplayStruct) }
           | MouseUpEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                          , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                          , eventButton :: Int, eventDisplay :: Ptr (DisplayStruct) }
           | MouseEnterDisplayEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                                    , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                                    , eventDisplay :: Ptr (DisplayStruct) }
           | MouseLeaveDisplayEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                                    , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                                    , eventDisplay :: Ptr (DisplayStruct) }
           | MouseWarpedEvent { eventMouseSource :: Mouse, eventTimestamp :: Double
                              , eventX :: Int, eventY :: Int, eventZ :: Int, eventW :: Int
                              , eventDX :: Int, eventDY :: Int, eventDZ :: Int, eventDW :: Int
                              , eventDisplay :: Ptr (DisplayStruct) }
           | TimerEvent { eventTimerSource :: Timer, eventTimestamp :: Double
                        , eventTimerCount :: Word64 }
           | DisplayExposeEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double
                                , eventX :: Int, eventY :: Int, eventDisplayWidth :: Int
                                , eventDisplayHeight :: Int }
           | DisplayResizeEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double
                                , eventX :: Int, eventY :: Int, eventDisplayWidth :: Int
                                , eventDisplayHeight :: Int }
           | DisplayCloseEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double }
           | DisplayLostEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double }
           | DisplayFoundEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double }
           | DisplaySwitchInEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double }
           | DisplaySwitchOutEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double }
           | DisplayOrientationEvent { eventDisplaySource :: Ptr (DisplayStruct), eventTimestamp :: Double
                                     , eventDisplayOrientation :: Int }
           | UserEvent { eventType :: Word, eventSource :: EventSource
                      , eventTimestamp :: Double, eventData1 :: Ptr (), eventData2 :: Ptr ()
                      , eventData3 :: Ptr (), eventData4 :: Ptr () }
	   deriving (Typeable, Data)

instance Storable Event where
	sizeOf _ = #{size ALLEGRO_EVENT}
	alignment _ = alignment (undefined :: Int)
	peek p = do typ <- #{peek ALLEGRO_EVENT, any.type} p :: IO (Word)
		    parseEvent typ p
	poke _ _ = undefined


createEventQueue :: IO (EventQueue)
createEventQueue = alCreateEventQueue >>= \q -> newForeignPtr alFinalizeEventQueue q 
foreign import ccall "allegro5/allegro.h al_create_event_queue"
	alCreateEventQueue :: IO (Ptr (EventQueueStruct))
foreign import ccall "allegro5/allegro.h &al_destroy_event_queue"
	alFinalizeEventQueue :: FunPtr (Ptr (EventQueueStruct) -> IO ())

destroyEventQueue :: EventQueue -> IO ()
destroyEventQueue q = withForeignPtr q alDestroyEventQueue
foreign import ccall "allegro5/allegro.h al_destroy_event_queue"
	alDestroyEventQueue :: Ptr (EventQueueStruct) -> IO ()

registerEventSource :: EventQueue -> EventSource -> IO ()
registerEventSource q source = withForeignPtr q $ \q' -> alRegisterEventSource q' source
foreign import ccall "allegro5/allegro.h al_register_event_source"
	alRegisterEventSource :: Ptr (EventQueueStruct) -> EventSource -> IO ()

unregisterEventSource :: EventQueue -> EventSource -> IO ()
unregisterEventSource q source = withForeignPtr q $ \q' -> alUnregisterEventSource q' source
foreign import ccall "allegro5/allegro.h al_unregister_event_source"
	alUnregisterEventSource :: Ptr (EventQueueStruct) -> EventSource -> IO ()

isEventQueueEmpty :: EventQueue -> IO (Bool)
isEventQueueEmpty q = liftM toBool $ withForeignPtr q alIsEventQueueEmpty
foreign import ccall "allegro5/allegro.h al_is_event_queue_empty"
	alIsEventQueueEmpty :: Ptr (EventQueueStruct) -> IO (CInt)

getNextEvent :: EventQueue -> IO (Maybe Event)
getNextEvent q = alloca $ \p -> do
	success <- liftM toBool $ withForeignPtr q $ \q' -> alGetNextEvent q' p
	event <- peek p :: IO (Event)
	return $ if success then Just event else Nothing
foreign import ccall "allegro5/allegro.h al_get_next_event"
	alGetNextEvent :: Ptr (EventQueueStruct) -> Ptr (Event) -> IO (CInt)

peekNextEvent :: EventQueue -> IO (Maybe Event)
peekNextEvent q = alloca $ \p -> do
	success <- liftM toBool $ withForeignPtr q $ \q' -> alPeekNextEvent q' p
	event <- peek p :: IO (Event)
	return $ if success then Just event else Nothing
foreign import ccall "allegro5/allegro.h al_peek_next_event"
	alPeekNextEvent :: Ptr (EventQueueStruct) -> Ptr (Event) -> IO (CInt)

dropNextEvent :: EventQueue -> IO (Bool)
dropNextEvent q = liftM toBool $ withForeignPtr q alDropNextEvent
foreign import ccall "allegro5/allegro.h al_drop_next_event"
	alDropNextEvent :: Ptr (EventQueueStruct) -> IO (CInt)

flushEventQueue :: EventQueue -> IO ()
flushEventQueue q = withForeignPtr q alFlushEventQueue
foreign import ccall "allegro5/allegro.h al_flush_event_queue"
	alFlushEventQueue :: Ptr (EventQueueStruct) -> IO ()

waitForEvent :: EventQueue -> Bool -> IO (Maybe Event)
waitForEvent q ret = if ret
	then liftM Just $ alloca $ \p -> do
		withForeignPtr q $ \q' -> alWaitForEvent q' p
		event <- peek p :: IO (Event)
		return event
	else withForeignPtr q $ \q' -> alWaitForEvent q' nullPtr >>= (\_ -> return Nothing)
foreign import ccall "allegro5/allegro.h al_wait_for_event"
	alWaitForEvent :: Ptr (EventQueueStruct) -> Ptr (Event) -> IO ()

-- This shit definitely needs to be fixed...
--
-- waitForEventTimed :: EventQueue -> Bool -> Float -> IO (Maybe Event)
-- waitForEventTimed queue ret secs = if ret
-- 	then liftM Just $ alloca f
-- 	else alWaitForEventTimed queue nullPtr secs >>= (\_ -> return Nothing)
-- 	where f :: Ptr (Event) -> IO (Event)
-- 	      f p = do
-- 		alWaitForEventTimed queue p secs
-- 		event <- peek p :: IO (Event)
-- 		return event
-- foreign import ccall "allegro5/allegro.h al_wait_for_event_timed"
-- 	alWaitForEventTimed :: EventQueue -> Ptr (Event) -> CFloat -> IO (CInt)

-- TODO: other event waiting methods

initUserEventSource :: EventSource -> IO ()
initUserEventSource = alInitUserEventSource
foreign import ccall "allegro5/allegro.h al_init_user_event_source"
	alInitUserEventSource :: EventSource -> IO ()

destroyUserEventSource :: EventSource -> IO ()
destroyUserEventSource = alDestroyUserEventSource
foreign import ccall "allegro5/allegro.h al_destroy_user_event_source"
	alDestroyUserEventSource :: EventSource -> IO ()

-- TODO: get/set event source data, unref user event

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
	eventKeyboardSource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Ptr (KeyboardStruct))
	eventTimestamp <- #{peek ALLEGRO_EVENT, any.timestamp} p :: IO (Double)
	eventDisplay <- #{peek ALLEGRO_EVENT, keyboard.display} p :: IO (Ptr (DisplayStruct))
	eventKeycode <- liftM Key (#{peek ALLEGRO_EVENT, keyboard.keycode} p :: IO (Int))
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
	eventDisplay <- #{peek ALLEGRO_EVENT, mouse.display} p :: IO (Ptr (DisplayStruct))
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
	eventDisplaySource <- #{peek ALLEGRO_EVENT, any.source} p :: IO (Ptr (DisplayStruct))
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

-- Get event sources

getDisplayEventSource :: Display -> IO (EventSource)
getDisplayEventSource d = withForeignPtr d alGetDisplayEventSource
foreign import ccall "allegro5/allegro.h al_get_display_event_source"
	alGetDisplayEventSource :: Ptr (DisplayStruct) -> IO (EventSource)

getKeyboardEventSource :: IO (EventSource)
getKeyboardEventSource = alGetKeyboardEventSource
foreign import ccall "allegro5/allegro.h al_get_keyboard_event_source"
	alGetKeyboardEventSource :: IO (EventSource)
