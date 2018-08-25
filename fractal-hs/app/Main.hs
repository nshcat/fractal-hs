-- TODO: load settings from json, save current command line to json

module Main where

import Configuration
import Data.Either.Combinators

main :: IO ()
main = do
        cfg <- retrieveSettings
        putStrLn $ case (mapBoth id show $ cfg) of 
           (Left s) -> s
           (Right s) -> s 
