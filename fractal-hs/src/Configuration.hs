{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration
    ( ImageSettings(..)
    , retrieveSettings
    , getParameters
    , ImageParameters(..)
    ) where
    
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Applicative
import GHC.Generics
import System.FilePath
import Control.Monad
import System.Directory
import Data.Time
import System.IO
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Semigroup ((<>))
import Data.Either.Combinators

-- TODO: Have ImageParameters as subrecord in ImageSettings. Figure that out!

-- | A record storing information about the image to be generated.
-- It is normally created by the command line argument parser and then
-- carried through the various parts of the program.
data ImageSettings = ImageSettings {
      imageWidth :: Int                 -- ^ The width of the image in pixels.
    , imageHeight :: Int                -- ^ The requested height of the image. 
    , imageMaxIters :: Int              -- ^ The maximum number of iterations the renderer will calculate for each pixel.
    , imageX0 :: Double                 -- ^ The complex numbers of the two corner vertices.
    , imageY0 :: Double             
    , imageX1 :: Double
    , imageAspectRatio :: Double        -- ^ The requested aspect ratio of the image
    , imageThreadCount :: Int           -- ^ The number of threads to use for image calculation
    , imageOutputPath :: String         -- ^ The output path for the image.
    , imageSettingsExportPath :: String -- ^ The path of the generated JSON file containing settings. If this is empty, no export will be done.
    } deriving (Show)
    
-- | Instance implementation for FromJSON. We can't use the pre-generated one here, since we need to ignore the last
-- field of the settings structure.
instance FromJSON ImageSettings where
  parseJSON = withObject "imageSettings" $ \o ->
    ImageSettings <$> o .: "width"
                  <*> o .: "height"
                  <*> o .: "max_iterations"
                  <*> o .: "x0"
                  <*> o .: "y0"
                  <*> o .: "x1"
                  <*> o .: "aspect_ratio"
                  <*> o .: "thread_count"
                  <*> o .: "output_path"
                  <*> pure ""
                 
-- | Instance implementation for ToJSON. We can't use the pre-generated one here, since we need to ignore the last
-- field of the settings structure.                 
instance ToJSON ImageSettings where
  toJSON s = object [
    "width" .= imageWidth s,
    "height" .= imageHeight s,
    "max_iterations" .= imageMaxIters s,
    "x0" .= imageX0 s,
    "y0" .= imageY0 s,
    "x1" .= imageX1 s,
    "aspect_ratio" .= imageAspectRatio s,
    "thread_count" .= imageThreadCount s,
    "output_path" .= imageOutputPath s ]
  

-- | A time used to handle the two different types of supported input: Loading image
-- settings from a JSON document and directly specifying them on the command line.   
data ProgramInput
  = JSONInput String
  | CommandlineInput ImageSettings


-- | A record similar to ImageSettings, but containing the result of adjustment calculations.
-- The values stored in this record are the final image parameters.
data ImageParameters = ImageParameters {
      paramWidth :: Int             -- ^ The width of the image in pixels.
    , paramHeight :: Int            -- ^ The requested height of the image. 
    , paramMaxIters :: Int          -- ^ The maximum number of iterations the renderer will calculate for each pixel.
    , paramX0 :: Double             -- ^ The complex numbers of the two corner vertices.
    , paramY0 :: Double             
    , paramX1 :: Double
    , paramY1 :: Double
    , paramAspectRatio :: Double    -- ^ The requested aspect ratio of the image
    } deriving (Show)
    
  
-- | Determine final image parameters from given image settings
getParameters :: ImageSettings -> ImageParameters
getParameters ImageSettings{..} = ImageParameters imageWidth h imageMaxIters imageX0 imageY0 imageX1 y1 imageAspectRatio
  where
    y1 = imageY0 + (imageX1 - imageX0) * imageAspectRatio
    h  = round . (* imageAspectRatio) . fromIntegral $ imageWidth


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
      <> value (sqrt 2)
      <> help "Aspect ratio" )
    <*> option auto
      (  long "thread-count"
      <> short 'T'
      <> value (-1)
      <> showDefault
      <> help "Number of threads to use for rendering. A value of -1 forces dynamic detection" )
    <*> strOption
      (  long "output"
      <> short 'o'
      <> metavar "NAME"
      <> value []
      <> help "Sets the output file path. If not supplied, a random filename in the current directory will be chosen" )
    <*> strOption
      (  long "export-json"
      <> short 'E'
      <> metavar "NAME"
      <> value []
      <> help "Sets the image settings JSON export file path. If not supplied, no export will be done." ))
     
      
-- | Combination of the two input parsers, supporting either input by file or command line,
-- but not both at the same time.
inputParser :: Parser ProgramInput
inputParser = jsonInputParser <|> commandlineInputParser


-- | Parse arguments and create image settings record value from them.
parseArguments :: IO(ProgramInput)
parseArguments = execParser opts
                   where opts   = info (inputParser <**> helper)
                                   ( fullDesc
                                  <> header "FractalHS -- Mandelbrot fractal renderer" )
                                  
-- | The top level settings retrieval function. Will first inspect the command line arguments,
-- and then either load the settings from JSON document or from the command line.
-- Additionally, this function will determine the output file name of the image file in
-- the case the user did not specify it.
retrieveSettings :: IO(Either String ImageSettings)
retrieveSettings = do
                     settings <- retrieveSettingsImpl
                     case settings of
                       r@(Left _) -> return r
                       r@(Right c) -> do
                                        -- Export settings if requested
                                        let exportPath = imageSettingsExportPath c
                                        when (not . null $ exportPath) $ writeFile exportPath $ (B.unpack . encodePretty $ c)
                                        
                                        -- If the user didnt supply an output path, we need to create one.
                                        -- We use the current time and date for this.
                                        if null . imageOutputPath $ c
                                          then (>>=) generateFileName $ return . Right . (\x -> c { imageOutputPath = x })
                                          else return $ Right c

-- | Generate a new image file name based on the current date and time. This is used when the user didn't supply
-- an explicit output path.
generateFileName :: IO(FilePath)
generateFileName = do
                     cwd <- getCurrentDirectory
                     tm <- getCurrentTime
                     let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" tm
                     return . (</>) cwd . (++) timestamp $ ".png"
  
                                        
-- | Inspect the command line arguments,
-- and then either load the settings from JSON document or from the command line.                    
retrieveSettingsImpl :: IO(Either String ImageSettings)
retrieveSettingsImpl = do
                     input <- parseArguments
                     case input of
                       (CommandlineInput x) -> return $ Right x
                       (JSONInput path)     -> do
                                                 contents <- readFile path
                                                 return $ maybeToRight "Failed to parse JSON document" . decode . B.pack $ contents
                                              
                                            

