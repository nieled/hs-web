module Adapter.HTTP.Main where

import qualified Adapter.HTTP.API.Auth as AuthAPI
import           Adapter.HTTP.Common
import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Web.Scotty.Trans

main :: ( MonadIO m
        , KatipContext m
        , AuthRepo m
        , EmailVerificationNotif m
        , SessionRepo m
        ) => Int -> (m Response -> IO Response ) -> IO ()
main port runner =
  scottyT port runner routes

routes :: ( MonadIO m
        , KatipContext m
        , AuthRepo m
        , EmailVerificationNotif m
        , SessionRepo m
        ) => ScottyT LText m ()
routes = do
  -- allows any response that is sent to the client to be compressed with Gzip,
  -- provided that the client sends a particular header
  middleware . gzip $ def { gzipFiles = GzipCompress }
  AuthAPI.routes

  -- catch and log any unknown exceptions, returning a 500
  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)
