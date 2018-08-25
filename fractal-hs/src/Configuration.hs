{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration
    ( ImageSettings(..)
    , retrieveSettings
    ) where
    
import Data.Aeson
import GHC.Generics
import System.IO
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Semigroup ((<>))
import Data.Either.Combinators

-- | A record storing information about the image to be generated.
-- It is normally created by the command line argument parser and then
-- carried through the various parts of the program.
data ImageSettings = ImageSettings {
      imageWidth :: Int             -- ^ The width of the image in pixels.
    , imageHeight :: Int            -- ^ The requested height of the image. 
    , imageMaxIters :: Int          -- ^ The maximum number of iterations the renderer will calculate for each pixel.
    , imageX0 :: Double             -- ^ The complex numbers of the two corner vertices.
    , imageY0 :: Double             
    , imageX1 :: Double
    , imageAspectRatio :: Double    -- ^ The requested aspect ratio of the image
    } deriving (Generic, Show, ToJSON, FromJSON)
    

-- | A time used to handle the two different types of supported input: Loading image
-- settings from a JSON document and directly specifying them on the command line.   
data ProgramInput
  = JSONInput String
  | CommandlineInput ImageSettings


-- | A command line parser that deals with JSON document program input
jsonInputParser :: Parser ProgramInput
jsonInputParser = JSONInput <$> strOption
  (  long "from-json"
  <> short 'j'
  <> metavar "FILENAME"
  <> help "Use JSON document as source for image settings" )
  
  
-- | A command line parser that deals with image settings that are supplied
-- via the command line
commandlineInputParser :: Parser ProgramInput
commandlineInputParser = CommandlineInput <$> (ImageSettings
    <$> option auto
      (  long "width"
      <> short 'w'
      <> metavar "WIDTH"
      <> value 500
      <> help "Width of the generated image" )
    <*> option auto
      (  long "height"
      <> short 'h'
      <> metavar "HEIGHT"
      <> value 500
      <> help "Height of the generated image. This is just a hint, and is modified to fit the aspect ratio." )
    <*> option auto
      (  long "max-iterations"
      <> short 'I'
      <> metavar "N"
      <> value 1200
      <> showDefault
      <> help "Maximum number of iterations per pixel" )
    <*> option auto
      (  long "x0"
      <> short 'x'
      <> value (-0.8228)
      <> help "Real component of first frame complex number" )
    <*> option auto
      (  long "y0"
      <> short 'y'
      <> value (-0.2087)
      <> help "Imaginary component of first frame complex number" )
    <*> option auto
      (  long "x1"
      <> short 'X'
      <> value (-0.8075)
      <> help "Real component of second frame complex number" )
    <*> option auto
      (  long "aspect-ratio"
      <> short 'A'
      <> value 1.4142135623730951
      <> help "Aspect ratio" ))
     
      
-- | Combination of the two input parsers, supporting either input by file or command line,
-- but not both at the same time.
inputParser :: Parser ProgramInput
inputParser = jsonInputParser <|> commandlineInputParser


-- | Parse arguments and create image settings record value from them.
parseArguments :: IO(ProgramInput)
parseArguments = execParser opts
                   where opts   = info (inputParser <**> helper)
                                   ( fullDesc
                                  <> progDesc "Mandelbrot fractal renderer"
                                  <> header "FractalHS" )
                                  
-- | The top level settings retrieval function. Will first inspect the command line arguments,
-- and then either load the settings from JSON document or from the command line.                                  
retrieveSettings :: IO(Either String ImageSettings)
retrieveSettings = do
                     input <- parseArguments
                     case input of
                       (CommandlineInput x) -> return $ Right x
                       (JSONInput path)     -> do
                                                 contents <- readFile path
                                                 return $ maybeToRight "Failed to parse JSON document" . decode . B.pack $ contents
                                              
                                            

