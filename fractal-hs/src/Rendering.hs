{-# LANGUAGE RecordWildCards #-}

module Rendering
    ( render
    ) where
    
import Configuration
import System.IO
import Data.Either.Combinators
import Data.Word      (Word8)
import Data.Complex   (Complex(..), magnitude)
import Codec.Picture  (generateImage, writePng, Pixel, Image)


fractal :: RealFloat a => Int -> Complex a -> Complex a -> Int -> (Complex a, Int)
fractal maxIters c z iter
    | iter >= maxIters = (1 :+ 1, 0)  -- invert values inside the holes
    | magnitude z > 2  = (z', iter)
    | otherwise        = fractal maxIters c z' (iter + 1)
  where
    z' = z * z + c


realize :: RealFloat a => Int -> (Complex a, Int) -> a
realize maxIters (z, iter) = (fromIntegral iter - log (log (magnitude z))) /
                     fromIntegral maxIters


render :: ImageParameters -> Int -> Int -> Word8
render ImageParameters{..} xi yi = grayify . realize paramMaxIters $ fractal paramMaxIters (x :+ y) (0 :+ 0) 0
  where
    (x, y)           = (trans paramX0 paramX1 paramWidth xi, trans paramY0 paramY1 paramHeight yi)
    trans n0 n1 a ni = (n1 - n0) * fromIntegral ni / fromIntegral a + n0
    grayify f        = truncate . (* 255) . sharpen $ 1 - f
    sharpen v        = 1 - exp (-exp ((v - 0.92) / 0.031))
    


    
