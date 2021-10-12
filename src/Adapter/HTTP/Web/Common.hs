module Adapter.HTTP.Web.Common where

import           Adapter.HTTP.Common ( getCurrentUserId )
import           ClassyPrelude
import           Domain.Auth ( SessionRepo, UserId )
import qualified Text.Blaze.Html.Renderer.Text as H
import           Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive.View as DF
import           Web.Scotty.Trans ( ActionT, ScottyError, html, redirect )

-- * Views

renderHtml :: (ScottyError e, Monad m) => H.Html -> ActionT e m ()
renderHtml = html . H.renderHtml

mainLayout :: Text -> H.Html -> H.Html
mainLayout title content =
  H.docTypeHtml $ do
    H.head $ do
      favicon "/images/logo.png"
      H.title $ H.toHtml title
    H.body $ do
      H.div $ H.img ! A.src "/images/logo.png"
      H.div content
  where
    favicon path =
      H.link ! A.rel "icon"
             ! A.type_ "image/png"
             ! A.href path

formLayout :: DF.View a -> Text -> H.Html -> H.Html
formLayout view action =
  H.form  ! A.method  "POST"
          ! A.enctype (H.toValue $ show $ DF.viewEncType view)
          ! A.action  (H.toValue action)

-- * Sessions

-- | It internally calls getCurrentUserId. However, it will redirect the user to the /auth/login endpoint if the user is not logged in.
reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mUserId <- getCurrentUserId
  case mUserId of
    Nothing ->
      redirect "/auth/login"
    Just userId ->
      return userId
