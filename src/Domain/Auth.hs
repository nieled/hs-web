{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Domain.Auth
  ( -- * Types
    Auth(..)
  , Email
  , mkEmail
  , rawEmail
  , Password
  , mkPassword
  , rawPassword
  , UserId
  , VerificationCode
  , SessionId
  , RegistrationError(..)
  , EmailVerificationError(..)
  , LoginError(..)

  -- * Ports
  , AuthRepo(..)
  , EmailVerificationNotif(..)
  , SessionRepo(..)

  -- * Use cases
  , register
  , verifyEmail
  , login
  , resolveSessionId
  , getUser
  ) where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Validation
import           Katip
    ( KatipContext, Severity (InfoS), katipAddContext, logTM, ls, sl )
import           Text.RawString.QQ

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq, Ord)

mkEmail :: Text -> Either [ErrMsg] Email
mkEmail = validate Email
  [ regexMatches
      [r|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
      "Not a valid email"
  ]

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

mkPassword :: Text -> Either [ErrMsg] Password
mkPassword x = validate Password
  [ lengthBetween 5 50 "Should between 5 and 50"
  , regexMatches [r|[0-9]|] "Should contain number"
  , regexMatches [r|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [r|[a-z]|] "Should contain lowercase letter"
  ]
  x

type VerificationCode = Text
type UserId = Int
type SessionId = Text
data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

rawPassword :: Password -> Text
rawPassword = passwordRaw

data RegistrationError
  = RegistrationErrorEmailToken
  deriving (Show, Eq)
data EmailValidationErr
  = EmailValidationErrInvalidEmail
  deriving (Show, Eq)
data PasswordValidationErr
  = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
  deriving (Show)
data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)
data LoginError
  = LoginErrorInvalidAuth
    | LoginErrorEmailNotVerified
  deriving (Show, Eq)

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode
                     -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

instance EmailVerificationNotif IO where
  notifyEmailVerification email vCode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vCode

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m)
         => Auth
         -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"

verifyEmail :: (KatipContext m, AuthRepo m)
            => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is verified successfully"
  return ()

login :: (KatipContext m, AuthRepo m, SessionRepo m)
      => Auth -> m (Either LoginError SessionId)
login auth =
  runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
      Nothing         -> throwError LoginErrorInvalidAuth
      Just (_, False) -> throwError LoginErrorEmailNotVerified
      Just (uId, _)   -> withUserIdContext uId . lift $ do
        sId <- newSession uId
        $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
        return sId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
