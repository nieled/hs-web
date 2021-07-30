module Adapter.Redis.Auth where

import           ClassyPrelude
import           Data.Has
import qualified Database.Redis as R
import qualified Domain.Auth as D
import           Text.StringRandom

type State = R.Connection

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
