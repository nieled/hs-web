{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    , run
    , MyException(..)
    , ServerException(..)
    ) where

import ClassyPrelude
import Language.Haskell.TH.Syntax (nameBase)
import Data.Aeson.TH

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ServerException
  = ServerOnFireException
  | ServerNotPluggedInException
  deriving (Show)
instance Exception ServerException

data MyException
  = ThisException
  | ThatException
  deriving (Show)
instance Exception MyException

run :: IO () -> IO ()
run action =
  action
    `catch` (\e -> putStrLn $ "ServerException: " <> tshow (e::ServerException))
    `catch` (\e -> putStrLn $ "MyException: " <> tshow (e::MyException))
    `catchAny` (putStrLn . tshow)

