#!/usr/bin/env nix-script-haskell
#!runtimeInputs fzf gitAndTools.gh
#!haskellPackages turtle aeson utf8-string

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe (mapMaybe)
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
      commits <- listCommits pr
      selected <- selectCommits commits
      liftIO $ print selected
      pure ()

data Commit = Commit {oid :: Text, messageHeadline :: Text}
  deriving (Generic, Show)

instance Aeson.FromJSON Commit

listCommits :: Int -> Shell [Commit]
listCommits pr =
  inprocJSON
    "gh"
    [ "pr",
      "view",
      Text.pack (show pr),
      "--json",
      "commits",
      "--jq",
      ".commits"
    ]
    ""

selectCommits :: [Commit] -> Shell [Commit]
selectCommits commits = do
  let sep = ": "
  let commitLines =
        mapMaybe textToLine $
          map
            (\Commit {oid, messageHeadline} -> oid <> sep <> messageHeadline)
            commits
  selected <- inproc "fzf" ["--multi"] (select commitLines)
  let selectedOids =
        fmap (\line -> fst (Text.breakOn sep line)) $
          Text.lines $ lineToText selected
  pure $
    filter (\Commit {oid} -> oid `elem` selectedOids) commits

inprocJSON :: Aeson.FromJSON a => Text -> [Text] -> Shell Line -> Shell a
inprocJSON command args stdin = do
  raw <- inproc command args stdin
  case Aeson.eitherDecodeStrict' (BS.fromString (Text.unpack (lineToText raw))) of
    Left err -> fail err
    Right parsed -> pure parsed
