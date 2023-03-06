#!/usr/bin/env nix-script-haskell
#!runtimeInputs fzf
#!haskellPackages turtle
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
  liftIO $ print maybePR
