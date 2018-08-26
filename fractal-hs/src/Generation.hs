{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module offering functions that can be used to generate an actual image using a
-- rendering function.
module Generation
    ( Partitionable(..)
    , generateImage
    ) where
    
import Configuration
import Data.Array
import Control.Parallel.Strategies 
import qualified Codec.Picture  as P
import GHC.Conc (numCapabilities)
    
class Ix a => Partitionable a where
    partition :: Int -> (a, a) -> [(a, a)]

    default partition :: (Num a) => Int -> (a, a) -> [(a, a)]
    partition n r@(l,_) = zipWith (\x y -> (x, x+y-1)) starts steps
        where
            (span, longerSpans) = rangeSize r `quotRem` n
            steps = zipWith (+) (replicate (min (rangeSize r) n) (fromIntegral span)) (replicate longerSpans 1 ++ repeat 0)
            starts = scanl (+) l steps
            
instance Partitionable Int

instance (Partitionable a, Partitionable b) => Partitionable (a, b) where
    partition n ((x0,y0), (x1, y1)) = do
        xr'@(x0', x1') <- partition n (x0, x1)
        let n' = n * rangeSize xr' `div` rangeSize (x0, x1)
        (y0', y1') <- partition n' (y0, y1)
        return ((x0', y0'), (x1', y1'))

-- | Create an array of given dimensions using given function that maps an index to an element.
-- This operation is done according to given parallel strategy.        
mkArrayPar :: (Partitionable i) => Int -> Strategy e -> (i, i) -> (i -> e) -> Array i e
mkArrayPar n s bounds f = listArray bounds (concat workUnits)
    where
        partitions = partition n bounds
        workUnits  = parMap (evalList s) (map f . range) partitions
        
-- | Create an image using given rendering function, using the number of threads specified in the image
-- settings.
generateImagePar :: forall a . P.Pixel a  => ImageParameters -> Int -> (Int -> Int -> a) -> P.Image a
generateImagePar ImageParameters{..} tc f = P.generateImage f' paramWidth paramHeight
    where
        bounds  = ((0, 0), (paramWidth-1,paramHeight-1))
        pixels  = mkArrayPar threads rseq bounds (uncurry f)
        f'      = curry (pixels !)
        threads = if tc <= 0 then numCapabilities else tc
        
-- | Create an image using given rendering function, using only a single thread.
generateImageSingle :: forall a . P.Pixel a  => ImageParameters -> (Int -> Int -> a) -> P.Image a
generateImageSingle ImageParameters{..} f = P.generateImage f paramWidth paramHeight


-- | Generate an image using given rendering function. If enabled by the supplied image settings,
-- this operation will be parallelized.
generateImage :: forall a . P.Pixel a => Int -> ImageParameters -> (Int -> Int -> a) -> P.Image a
generateImage 1 p f = generateImageSingle p f
generateImage t p f = generateImagePar p t f

        
        
    
        


