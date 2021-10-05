module Adapter.HTTP.API.Auth where

import           Adapter.HTTP.Common
import           ClassyPrelude
import           Data.Aeson hiding ( json, (.:) )
import           Domain.Auth
import           Katip
import Network.HTTP.Types.Status ()
import           Text.Digestive.Form ( (.:) )
import qualified Text.Digestive.Form as DF
import           Web.Scotty.Trans

routes :: (ScottyError e, MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif  m, SessionRepo m) => ScottyT e m ()
routes = do
  -- register
  post "/api/auth/register" undefined

  -- verify email
  post "/api/auth/verifyEmail" undefined

  -- login
  post "/api/auth/login" undefined

  -- get user
  get "/api/users" undefined

