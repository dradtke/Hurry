module Main where

import Allegro.System

main :: IO ()
main = do
	setAppName "SS Allegro"
	initialize
	name <- getAppName
	putStrLn $ "app name: " ++ name
