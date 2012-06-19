{-
 - Entry point for testing Hurry.
 -}

module Main where

import Allegro.Display
import Allegro.Event
import Allegro.Keyboard
import Allegro.System

import Data.Data
import Data.Maybe

data Game = Game { {- put global variables here -} }

main :: IO ()
main = do
	initAllegro
	installKeyboard
	eventQueue <- createEventQueue
	display <- createDisplay 640 480
	getDisplayEventSource display >>= registerEventSource eventQueue
	getKeyboardEventSource >>= registerEventSource eventQueue
	loop eventQueue

loop :: EventQueue -> IO ()
loop eventQueue = do
	event <- waitForEvent eventQueue True
	case event of
		Nothing -> loop eventQueue
		Just e -> do
			let name = show $ toConstr e
			case name of
				"DisplayCloseEvent" -> putStrLn "exiting."
				"KeyDownEvent" -> do
					keyName <- keycodeToName $ eventKeycode e
					putStrLn $ "key pressed: " ++ keyName
					loop eventQueue
				"KeyCharEvent" -> loop eventQueue
				"KeyUpEvent" -> loop eventQueue
				_ -> do
					putStrLn $ "unknown event: " ++ name
					loop eventQueue
