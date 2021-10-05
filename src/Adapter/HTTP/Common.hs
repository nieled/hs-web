module Adapter.HTTP.Common where

import           Blaze.ByteString.Builder ( toLazyByteString )
import           ClassyPrelude
import           Data.Aeson hiding ( json )
import           Data.Time.Lens
import           Domain.Auth
import           Network.HTTP.Types.Status
import qualified Text.Digestive.Aeson as DF
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import           Web.Cookie
import           Web.Scotty.Trans

-- * Forms

-- | parse JSON from the HTTP request body and run the form
-- If the validation results in an error, we want to
-- immediately respond with 400, indicating a bad request,
-- and put the error messages in the HTTP response body.
parseAndValidateJSON :: (ScottyError e, MonadIO m, ToJSON v) => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) ->
      return result

-- | Used by digestive-functors for form validation
toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success

-- * Cookies

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie =
  setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val

-- * Sessions

-- | Set the cookie that is going to be sent to the client
setSessionIdInCookie :: (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $ def { setCookieName = "sId"
                  , setCookiePath = Just "/"
                  , setCookieValue = encodeUtf8 sId
                  , setCookieExpires = Just $ modL month (+ 1) curTime
                  , setCookieHttpOnly = True
                  , setCookieSecure = False
                  , setCookieSameSite = Just sameSiteLax
                  }

-- | Used to infer the current UserId from the HTTP request.
getCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing  -> return Nothing
    Just sId -> lift $ resolveSessionId sId

-- | Similar to `getCurrentUserId`.
-- The difference is that we just respond with 401 and finish
-- processing the request.
reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUserId <- getCurrentUserId
  case mayUserId of
    Nothing -> do
      status status401
      json ("AuthRequired"::Text)
      finish
    Just userId -> return userId
