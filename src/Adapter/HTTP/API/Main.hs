module Adapter.HTTP.API.Main where

import qualified Adapter.HTTP.API.Auth as Auth
import           Adapter.HTTP.API.Common
import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Web.Scotty.Trans

main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) => (m Response -> IO Response) -> IO Application
main runner =
  scottyAppT runner routes

routes :: ( MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) => ScottyT LText m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}

  Auth.routes

  notFound $ do
    status status404
    json $ errorResponse ("NotFound" :: Text)

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json $ errorResponse ("InternalServerError" :: Text)

