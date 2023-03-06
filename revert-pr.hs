#!/usr/bin/env nix-script-haskell
#!haskellPackages turtle
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude

main :: IO ()
main =
  echo "Hello, 🐢!"
