module Adapter.HTTP.Main where

import qualified Adapter.HTTP.API.Main as API
import qualified Adapter.HTTP.Web.Main as Web
import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Vhost

main :: ( MonadIO m
        , KatipContext m
        , AuthRepo m
        , EmailVerificationNotif m
        , SessionRepo m
        ) => Int -> (m Response -> IO Response ) -> IO ()
main port runner = do
  web <- Web.main runner
  api <- API.main runner
  run port $ vhost [(pathBeginsWith "api", api)] web
  where
    pathBeginsWith path req = headMay (pathInfo req) == Just path
