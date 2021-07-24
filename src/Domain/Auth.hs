module Domain.Auth where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Validation
import           Text.Regex.PCRE.Heavy

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)
newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)
data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Show, Eq)
type VerificationCode = Text
type UserId = Int
type SessionId = Text

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
      [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
      "Not a valid email"
  ]

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50 "Should be between 5 and 50"
  , regexMatches [re|\d|] "Should contain number"
  , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]

data RegistrationError = RegistrationErrorEmailToken
  deriving (Show, Eq)
data EmailValidationErr = EmailValidationErrInvalidEmail
data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
data EmailVerificationError
  = EmailVerificationErrorInvalidCode
      deriving (Show, Eq)
data LoginError = LoginErrorInvalidAuth
    | LoginErrorNotVerified
      deriving (Show, Eq)

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "adding auth: " <> rawEmail email
    return $ Right "fake verification code"

instance EmailVerificationNotif IO where
  notifyEmailVerification email vCode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vCode

register
  :: (AuthRepo m, EmailVerificationNotif m)
  => Auth
  -> m (Either RegistrationError ())
register auth =
  runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth =
  runExceptT $ do
    result <- lift $ findUserByAuth auth
    -- case result of
    undefined

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId