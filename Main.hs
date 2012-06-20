{-
 - Entry point for testing Hurry.
 -}

module Main where

import Allegro.Display
import Allegro.Event
import Allegro.Keyboard
import Allegro.Mouse
import Allegro.System

import Control.Monad
import Data.Data
import Data.Maybe

data Game = Game { {- put global variables here -} }

main :: IO ()
main = do
	-- Initialize Allegro
	initAllegro
	installKeyboard
	installMouse

	-- Create the Display
	display <- createDisplay 640 480

	-- Set up the event queue and register sources
	eventQueue <- createEventQueue
	getDisplayEventSource display >>= registerEventSource eventQueue
	getKeyboardEventSource >>= registerEventSource eventQueue
	getMouseEventSource >>= registerEventSource eventQueue

	-- Main loop
	loop eventQueue

	-- Clean up
	destroyEventQueue eventQueue
	destroyDisplay display

-- This should take a game state object instead
loop :: EventQueue -> IO ()
loop eventQueue = do
	event <- waitForEvent eventQueue True
	case event of
		Nothing -> loop eventQueue
		Just e -> do
			running <- handleEvent (show $ toConstr e) e
			when running $ loop eventQueue

-- Returns True if the game should keep running, otherwise False
handleEvent :: String -> Event -> IO (Bool)

handleEvent "DisplayCloseEvent" e = do
	putStrLn "exiting."
	return False

handleEvent "KeyDownEvent" e = do
	keyName <- keycodeToName $ eventKeycode e
	putStrLn $ "key pressed: " ++ keyName
	return True

handleEvent "MouseDownEvent" e = do
	(x,y) <- getMousePosition
	putStrLn $ "mouse clicked: (" ++ show x ++ "," ++ show y ++ ")"
	return True

handleEvent "KeyCharEvent" e 		= return True
handleEvent "KeyUpEvent" e 		= return True
handleEvent "MouseAxesEvent" e 		= return True
handleEvent "MouseLeaveDisplayEvent" e 	= return True
handleEvent "MouseEnterDisplayEvent" e 	= return True
handleEvent "MouseUpEvent" e 		= return True
handleEvent name e = do
	putStrLn $ "unknown event: " ++ name
	return True
