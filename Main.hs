{-
 - Entry point for testing Hurry.
 -}

module Main where

import Allegro.Display
import Allegro.Event
import Allegro.System
import Control.Monad.State

data Game = Game { {- put global variables here -} }

main :: IO ()
main = do
	initAllegro
	queue <- createEventQueue
	display <- createDisplay 640 480
	getDisplayEventSource display >>= registerEventSource queue
	let game = Game in loop game
	return ()

loop :: State Game -> IO ()
loop state = return ()
