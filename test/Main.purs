module Test.Main where

import Prelude
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error)
import Data.Array (find)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Image.Decoder (Image, decode)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Network.HTTP.Affjax (AJAX, AffjaxResponse, get)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)


main :: forall e. Eff (exception :: EXCEPTION, ajax :: AJAX, console :: CONSOLE | e) Unit
main = void $ launchAff do
  tst "https://upload.wikimedia.org/wikipedia/en/2/24/Lenna.png"
  tst "https://vignette2.wikia.nocookie.net/computervision/images/3/34/Lenna.jpg"
  where contentType :: Array ResponseHeader -> Either Error String
        contentType hs = case find (\h -> "content-type" == responseHeaderName h) hs of
          Just h -> Right $ responseHeaderValue h
          Nothing -> Left $ error "missing content-type"
        dec :: AffjaxResponse ArrayBuffer -> Either Error Image
        dec d = MediaType <$> contentType d.headers >>= decode d.response
        tst url = do
          response <- attempt $ get url
          case response of
            Left err -> log $ "Failed to fetch image: " <> show err
            Right res -> logShow $ dec res
          pure unit
