module Domain.Registration where

import ClassyPrelude
import Domain.Auth

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

