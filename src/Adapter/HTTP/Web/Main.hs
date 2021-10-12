module Adapter.HTTP.Web.Main where

import qualified Adapter.HTTP.Web.Auth as Auth
import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Static
import           Web.Scotty.Trans

main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)
  => (m Response -> IO Response) -> IO Application
main runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT runner $ routes cacheContainer

routes :: ( MonadIO m
          , KatipContext m
          , AuthRepo m
          , EmailVerificationNotif m
          , SessionRepo m
          )
  => CacheContainer
  -> ScottyT LText  m ()
routes cachingStrategy = do
  middleware $
    gzip $ def { gzipFiles = GzipCompress }
  middleware $
    staticPolicyWithOptions
      defaultOptions { cacheContainer = cachingStrategy }
      (addBase "src/Adapter/HTTP/Web")

  Auth.routes

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    text "Internal server error!"
