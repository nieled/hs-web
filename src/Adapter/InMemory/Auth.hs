module Adapter.InMemory.Auth where

import           ClassyPrelude
import qualified Domain.Auth as D

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
initialState = undefined