{-
 - Entry point for testing Hurry.
 -}

module Main where

import Allegro.Display
import Allegro.Event
import Allegro.System

import Data.Data
import Data.Maybe

data Game = Game { {- put global variables here -} }

main :: IO ()
main = do
	initAllegro
	eventQueue <- createEventQueue
	display <- createDisplay 640 480
	getDisplayEventSource display >>= registerEventSource eventQueue
	loop eventQueue
	return ()

loop :: EventQueue -> IO ()
loop eventQueue = do
	event <- waitForEvent eventQueue True
	case event of
		Nothing -> loop eventQueue
		Just e -> do
			let name = show $ toConstr e
			case name of
				"DisplayCloseEvent" -> putStrLn "exiting."
				_ -> do
					putStrLn $ "unknown event: " ++ name
					loop eventQueue
