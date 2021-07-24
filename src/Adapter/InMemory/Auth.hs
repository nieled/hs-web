module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Data.Has
import qualified Domain.Auth as D
import           Text.StringRandom

-- | The following computation works for any m that is an instance
-- of MonadIO and MonadReader r, where r is any structure that has
-- TVar State.
-- We can delcare this way thanks to language ex: ConstraintKinds
-- InMemory r m
--
-- We need MonadIO because we need to do IO, such as changing the
-- content of the TVar and generating a random string.
--
-- We need the Has (TVar State) r, MonadReader r m constraint
-- because in each function we need access to the state.
-- In this case, we choose to thread the state through MonadReader.
-- Otherwise, we need to pass in the state as a function argument,
-- such as:
-- setEmailAsVerified :: TVar State
--                    -> VerificationCode -> IO (Either EmailVerification
--                    Error ())
--
-- The implication of such function design is that whoever calls
-- the function needs to explicitly pass the state. Once you need
-- to call such functions deep in the call chain, those functions
-- get unwieldly pretty fast.
type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State
  = State
      { stateAuths            :: [(D.UserId, D.Auth)]
      , stateUnverifiedEmails :: Map D.VerificationCode D.Email
      , stateVerifiedEmails   :: Set D.Email
      , stateUserIdCounter    :: Int
      , stateNotifications    :: Map D.Email D.VerificationCode
      , stateSessions         :: Map D.SessionId D.UserId
      }
  deriving (Show, Eq)

initialState :: State
initialState = State
  { stateAuths = []
  , stateUnverifiedEmails = mempty
  , stateVerifiedEmails = mempty
  , stateUserIdCounter = 0
  , stateNotifications = mempty
  , stateSessions = mempty
  }

addAuth :: InMemory r m
        => D.Auth -> m (Either D.RegistrationError D.VerificationCode )
addAuth = undefined

setEmailAsVerified :: InMemory r m
                   => D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m
               => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m
                    => D.UserId -> m (Maybe D.Email)
findEmailFromUserId = undefined

-- Only for tests
getNotificationsForEmail :: InMemory r m
                         => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state

notifyEmailVerification :: InMemory r m
                        => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState = state { stateNotifications = newNotifications }
    writeTVar tvar newState

newSession :: InMemory r m
           => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = insertMap sId uId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId

findUserIdBySessionId :: InMemory r m
                      => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

