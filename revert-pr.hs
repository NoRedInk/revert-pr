#!/usr/bin/env nix-script-haskell
#!runtimeInputs fzf
#!haskellPackages turtle
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude

main :: IO ()
main = sh $ do
  response <-
    inproc
      "fzf"
      [ "--header",
        "Are turtles cool?"
      ]
      ("yes" <|> "indeed")
  echo ("You answered: " <> response)
