{-# OPTIONS_GHC -Wno-deprecations #-}
module Lib
    ( main
    ) where

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.Redis.Auth as Redis
import           ClassyPrelude
import           Control.Monad
import           Control.Monad.Catch
import           Domain.Auth
import           Katip
import           Text.StringRandom

type State = (PG.State, Redis.State, MQ.State, TVar M.State)

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO
              , KatipContext, Katip, MonadThrow, MonadCatch, MonadUnliftIO)

run :: LogEnv -> State -> App a -> IO a
run le state
  = runKatipContextT le () mempty
    . flip runReaderT state
    . unApp

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  -- notifyEmailVerification = M.notifyEmailVerification
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance MonadFail App where
  fail = error

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = ClassyPrelude.bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HS-Web" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

withState :: (LogEnv -> State -> IO ()) -> IO ()
withState action =
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action le state
  where
    -- TODO: Improve this by parsing configuration from environment
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"
    redisCfg = "redis://localhost:6379/0"
    pgCfg =
      PG.Config
      { PG.configUrl = "postgresql://localhost/hs-web"
      , PG.configStripeCount = 2
      , PG.configMaxOpenConnPerStripe = 5
      , PG.configIdleConnTimeout = 10
      }

main :: IO ()
main =
  withState $ \le state@(_, _, mqState ,_) -> do
    let runner = run le state
    MQAuth.init mqState runner
    runner action

action :: App ()
action = do
  randEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@test\\.com"
  let email = either undefined id $ mkEmail randEmail
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth = Auth email passw
  register auth
  vCode <- pollNotif email
  verifyEmail vCode
  Right session        <- login auth
  Just uId             <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
  where
    pollNotif email = do
      result <- M.getNotificationsForEmail email
      case result of
        Nothing    -> pollNotif email
        Just vCode -> return vCode
