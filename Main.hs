{-
 - Entry point for testing Hurry.
 -}

module Main where

import Allegro.Event
import Allegro.System

main :: IO ()
main = do
	setAppName "SS Allegro"
	initialize
	name <- getAppName
	putStrLn $ "app name: " ++ name
