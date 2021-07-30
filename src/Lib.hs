module Lib
    ( someFunc
    ) where

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import           ClassyPrelude
import           Control.Monad
import           Domain.Auth
import           Katip

someFunc :: IO ()
someFunc = withKatip $ \le -> do
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> run le (pgState, mState) action
  where
    -- TODO: Improve this by parsing configuration from environment
    pgCfg = PG.Config
            { PG.configUrl = "postgresql://localhost/hs-web"
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe = 5
            , PG.configIdleConnTimeout = 10
            }

action :: App ()
action = do
  let Right email = mkEmail "ecky@test.com"
      Right passw = mkPassword "1234ABCDefgh"
      auth = Auth email passw
  _ <- register auth
  Just vCode <- M.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)

type State = (PG.State, TVar M.State)
newtype App a = App
  -- Same as KatipContextT (ReaderT State IO)
  -- TODO: Should we derive MonadThrow?
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO
              , KatipContext, Katip)

run :: LogEnv -> State -> App a -> IO a
run le state
  = runKatipContextT le () mempty
    . flip runReaderT state
    . unApp

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HS-Web" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

instance MonadFail App where
  fail = error
