module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration

type State = Pool Connection
data Config
  = Config
      { configUrl              :: ByteString
      , configStripeCount      :: Int
      , configMaxOpenPerStripe :: Int
      , configIdleConnTimeout  :: NominalDiffTime
      }

-- | Internally calls the `bracket` function.
-- The pool has a lifetime, that is, it needs to be
-- destroyed after use, then it's best to manage
-- the creation and destruction with `bracket`.
withPool :: Config -> (State -> IO a) -> IO a
withPool cfg =
  bracket initPool cleanPool
  where
    initPool = createPool openConn closeConn
                (configStripeCount cfg)
                (configIdleConnTimeout cfg)
                (configMaxOpenPerStripe cfg)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close

-- | Internally calls withPool and immediately
-- executes database migration before continuing
-- on executing action from the function parameter.
-- It should be execued during startup.
withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action state

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
  where
    cmds = [ MigrationInitialization
           , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
           ]
