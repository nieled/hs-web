module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Control.Monad.Except
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

-- | First, we generate a random VerificationCode using the similar
-- mechanism as generating SessionId.
-- Then, we check whether the email is a duplicate by traversing stateAuths.
-- If it is a duplicate, we return a RegistrationErrorEmailTaken error.
-- Otherwise, we continue to insert the user’s Auth into stateAuths.
-- UserId is generated using a counter. We simply increment the counter
-- by one when generating a new UserId.
-- Since we also want users to verify their email, we store the Email
-- along with VerificationCode in stateUnverifiedEmails.
addAuth :: InMemory r m
        => D.Auth
        -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  -- gen verification code
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- check wether given email is duplicate
    let auths = stateAuths state
        email = D.authEmail auth
        isDuplicate = any ((email ==) . (D.authEmail . snd)) auths
    when isDuplicate $ throwError D.RegistrationErrorEmailToken
    -- update the state
    let newUserId = stateUserIdCounter state + 1
        newAuths = (newUserId, auth) : auths
        unverifieds = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode email unverifieds
        newState = state
          { stateAuths = newAuths
          , stateUserIdCounter = newUserId
          , stateUnverifiedEmails = newUnverifieds
          }
    lift $ writeTVar tvar newState
    return (newUserId, vCode)

-- | The basic idea is to look up an Email in stateUnverifiedEmails
-- from the given VerificationCode and move it into stateVerifiedEmails.
-- Since VerificationCode might not map to any Email, we may throw
-- EmailVerificationErrorInvalidCode.
setEmailAsVerified :: InMemory r m
                   => D.VerificationCode
                   -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        mayEmail = lookup vCode unverifieds
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let auths = stateAuths state
        mayUserId = map fst . find ((email ==) . D.authEmail . snd) $ auths
    uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let verifieds = stateVerifiedEmails state
        newVerifieds = insertSet email verifieds
        newUnverifieds = deleteMap vCode unverifieds
        newState = state
          { stateUnverifiedEmails = newUnverifieds
          , stateVerifiedEmails = newVerifieds
          }
    lift $ writeTVar tvar newState
    return (uId, email)

-- | We first need to look up UserId from the given Auth.
-- If such Auth is not found, then return Nothing.
-- If it’s found, then we need to check whether the Email is
-- verified or not by checking with stateVerifiedEmails.
-- The result of this check is then put into the return value.
findUserByAuth :: InMemory r m
               => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = map fst . find ((auth ==) . snd) $ stateAuths state
  case mayUserId of
    Nothing -> return Nothing
    (Just uId) -> do
      let verifieds = stateVerifiedEmails state
          email = D.authEmail auth
          isVerified = email `elem` verifieds
      return $ Just (uId, isVerified)

-- | Read stateAuths and find one entry that matches the given UserId
-- and get the Email out of that entry
findEmailFromUserId :: InMemory r m
                    => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> mayAuth

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

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = return a