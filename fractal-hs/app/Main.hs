{-# LANGUAGE RecordWildCards #-}
module Main where

import Configuration
import Generation
import Rendering
import Text.Printf
import Codec.Picture  (writePng)

main :: IO ()
main = do
         ret <- retrieveSettings
         case ret of
           (Left s) -> putStrLn $ "error: " ++ s
           (Right cfg@ImageSettings{..}) -> do       
                                                let imgParams = getParameters cfg
                                                let width = paramWidth imgParams
                                                let height = paramHeight imgParams
                                                printf "Width: %d, Height: %d\n" width height
                                                writePng imageOutputPath $ generateImage imageThreadCount imgParams $ render imgParams
