module Adapter.HTTP.Web.Auth where

import           Adapter.HTTP.Common
import           Adapter.HTTP.Web.Common
import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive.Blaze.Html5 as DH
import           Text.Digestive.Form ( (.:) )
import qualified Text.Digestive.Form as DF
import           Text.Digestive.Scotty
import qualified Text.Digestive.View as DF
import           Web.Scotty.Trans

-- * Routes

routes :: ( ScottyError e, MonadIO m)
  => ScottyT e m ()
routes = do
  -- home
  get "/" $
    redirect "/users"

  -- register
  get "/auth/register" undefined
  post "/auth/register" undefined

  -- verify email
  get "/auth/verifyEmail/:code" undefined

  -- login
  get "/auth/login" undefined
  post "/auth/login" undefined

  -- get user
  get "/users" undefined