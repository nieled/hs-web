module Adapter.HTTP.Web.Main where

import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Network.Wai
import           Web.Scotty.Trans

main :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)
  => (m Response -> IO Response) -> IO Application
main runner =
  scottyAppT runner routes

routes :: (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)
  => ScottyT LText  m ()
routes = do
  get "/" $
    text "Hello from Web!"

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    text "Internal server error!"
