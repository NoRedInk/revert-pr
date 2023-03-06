#!/usr/bin/env nix-script-haskell
#!runtimeInputs fzf
#!haskellPackages turtle aeson
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Turtle
import Prelude

newtype Options = Options {maybePR :: Maybe Int}
  deriving (Show)

optionsParser :: Parser Options
optionsParser = do
  maybePR <- optional (argInt "PR_NUMBER" "which PR to revert")
  pure Options {maybePR}

main :: IO ()
main = sh $ do
  Options {maybePR} <- options "Revert a PR" optionsParser
  case maybePR of
    Nothing -> echo "TODO"
    Just pr -> do
      _ <- listCommits pr
      pure ()

data Commit = Commit {oid :: String, messageHeadline :: String}
  deriving (Generic, Show)

instance Aeson.FromJSON Commit

listCommits :: Int -> Shell [Commit]
listCommits pr = do
  raw <-
    inproc
      "gh"
      [ "pr",
        "view",
        (Text.pack (show pr)),
        "--json",
        "commits",
        "--jq",
        ".commits"
      ]
      ""
  liftIO (print raw)
  pure []
