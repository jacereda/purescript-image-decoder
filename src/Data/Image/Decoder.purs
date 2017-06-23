module Data.Image.Decoder where

import Prelude
import Data.Image.JPEG.Decoder as JPEG
import Data.Image.PNG.Decoder as PNG
import Control.Monad.Eff.Exception (Error, error)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.MediaType (MediaType(..))

newtype Image = Image
  { data :: ArrayBuffer
  , width :: Int
  , height :: Int
  }

instance showImage :: Show Image where
  show (Image i) = show i.width <> "x" <> show i.height <> " image"

decode :: ArrayBuffer -> MediaType -> Either Error Image
decode ab mt =
  Image <$> case mt of
    (MediaType "image/png") -> PNG.decode ab
    (MediaType "image/jpeg") -> JPEG.decode ab
    t -> Left $ error $ "Unsupported image format: " <> show t
