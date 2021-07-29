module Lib
    ( someFunc
    ) where

import qualified Adapter.InMemory.Auth as M
import           ClassyPrelude
import           Control.Monad
import           Domain.Auth
import           Katip

someFunc :: IO ()
someFunc = withKatip $ \le -> do
  state <- newTVarIO M.initialState
  run le state action

action :: App ()
action = do
  let email = either undefined id $ mkEmail "ecky@test.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth = Auth email passw
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)

type State = TVar M.State
newtype App a = App
  -- Same as KatipContextT (ReaderT State IO)
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
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

instance MonadFail App where
  fail = undefined
