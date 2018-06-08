module Test.Main where

import Prelude
import Effect.Aff (attempt, launchAff_)
import Effect.Class.Console (log, logShow)
import Effect (Effect)
import Effect.Exception (Error, error)
import Data.Array (find)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Image.Decoder (Image, decode)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Network.HTTP.Affjax (AffjaxResponse, get)
import Network.HTTP.Affjax.Response as Response
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)


main :: Effect Unit
main = launchAff_ do
  tst "https://upload.wikimedia.org/wikipedia/en/7/7d/Lenna_%28test_image%29.png"
  tst "https://vignette2.wikia.nocookie.net/computervision/images/3/34/Lenna.jpg"
  where contentType :: Array ResponseHeader -> Either Error String
        contentType hs = case find (\h -> "content-type" == responseHeaderName h) hs of
          Just h -> Right $ responseHeaderValue h
          Nothing -> Left $ error "missing content-type"
        dec :: AffjaxResponse ArrayBuffer -> Either Error Image
        dec d = MediaType <$> contentType d.headers >>= decode d.response
        tst url = do
          response <- attempt $ get Response.arrayBuffer url
          case response of
            Left err -> log $ "Failed to fetch image: " <> show err
            Right res -> logShow $ dec res
          pure unit
