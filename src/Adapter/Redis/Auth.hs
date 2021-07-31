module Adapter.Redis.Auth where

import           ClassyPrelude
import           Data.Has
import qualified Database.Redis as R
import qualified Domain.Auth as D
import           Text.StringRandom

-- | type synonym for R.Connection. This is for consistency with the existing
-- PostgreSQL implementation as well as for future proofing
type State = R.Connection

-- TODO: Add MonadThrow
type Redis r m = (Has State r, MonadReader r m, MonadIO m)

-- | Create state from redis url string
-- format: redis://user:pass@host:port/db
-- sample: redis://abc:def@localhost:6379/0
withState :: String -> (State -> IO a) -> IO a
withState connUrl action = do
  case R.parseConnectInfo connUrl of
    Left _ -> throwString "Invalid Redis conn URL"
    Right connInfo -> do
      conn <- R.checkedConnect connInfo
      action conn

-- | Helper function to execute R.Redis under Redis r m constraint
-- Gets the connection from the environment, then execute the R.Redis action
-- using the R.runRedis function
withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action

-- | Create a random alphanumeric `SessionId`. Then store the mapping between
-- the `SessionId` and `UserId` from the function parameter to Redis
newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
  sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ userId)
  case result of
    Right R.Ok -> return sId
    err        -> throwString $ "Unexpected Redis error: " <> show err

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  result <- withConn $ R.get (encodeUtf8 sId)
  case result of
    Right (Just uIdStr) -> return $ readMay . unpack . decodeUtf8 $ uIdStr
    err -> throwString $ "Unexpected Redis error: " <> show err
