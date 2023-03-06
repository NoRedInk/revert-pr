#!/usr/bin/env nix-script-haskell
#!runtimeInputs fzf gitAndTools.gh git
#!haskellPackages turtle aeson utf8-string

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe (mapMaybe, maybe)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
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
  pr <- maybe selectPR pure maybePR
  commits <- listCommits pr
  toRevert <- selectCommits commits
  withStash $ do
    echo "Starting to revert commits..."
    revertedCommits <- traverse revertCommit toRevert
    addAll
    commitRevert pr revertedCommits

selectPR :: Shell Int
selectPR = do
  raw <- returnsList $ inshell "gh pr list --state merged" ""
  selected <- inshell "fzf" (select raw)
  pure $ read $ Text.unpack $ head $ Text.words $ lineToText selected

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
  selected <-
    returnsList $
      inproc
        "fzf"
        [ "--multi",
          "--no-sort",
          "--bind",
          "ctrl-a:select-all",
          "--preview",
          "git show --color=always {+1}",
          "--delimiter",
          " ",
          "--with-nth",
          "2..",
          "--header",
          "Select commits to revert (ctrl-a to select all)"
        ]
        (select commitLines)
  let selectedOids =
        fmap (\line -> fst (Text.breakOn sep line)) $
          Text.lines $ linesToText selected
  pure $
    filter (\Commit {oid} -> oid `elem` selectedOids) commits

withStash :: Shell () -> Shell ()
withStash go = do
  unstaged <- hasUnstagedChanges
  if unstaged
    then do
      _ <- stash
      _ <- go
      popStash
    else go

hasUnstagedChanges :: Shell Bool
hasUnstagedChanges = do
  response <- inshell "git status --porcelain" ""
  pure (response /= "")

stash :: Shell ()
stash = do
  echo "Stashing unstaged changes..."
  shells "git stash -u" ""

popStash :: Shell ()
popStash = do
  echo "Popping stash..."
  shells "git stash pop" ""

data Action = Reverted | ResolvedConflicts | SkippedMerge
  deriving (Show)

revertCommit :: Commit -> Shell (Action, Commit)
revertCommit commit = do
  isMerge <- isMergeCommit commit
  let commitText = oid commit <> " " <> messageHeadline commit <> "..."
  if isMerge
    then do
      echoText ("Skipping merge commit " <> commitText)
      pure (SkippedMerge, commit)
    else do
      echoText ("Reverting " <> commitText)
      (exitCode, _, _) <- procStrictWithErr "git" ["revert", "-n", oid commit, "--strategy-option", "ours"] ""
      case exitCode of
        ExitSuccess -> pure (Reverted, commit)
        ExitFailure _ -> do
          _ <- shells "git mergetool" ""
          echoText ("Resolved conflicts for " <> commitText)
          pure (ResolvedConflicts, commit)

isMergeCommit :: Commit -> Shell Bool
isMergeCommit commit = do
  let sha = oid commit
  (_, response, _) <- procStrictWithErr "git" ["rev-list", "-1", "--merges", oid commit <> "~1.." <> oid commit] ""
  pure (response /= "")

addAll :: Shell ()
addAll =
  shells "git add --all" ""

commitRevert :: Int -> [(Action, Commit)] -> Shell ()
commitRevert pr actionCommit = do
  let message = Text.unlines (("Revert #" <> Text.pack (show pr) <> "\n") : map renderActionCommit actionCommit)
  echo "Committing revert..."
  procs "git" ["commit", "--message", message] ""

renderActionCommit :: (Action, Commit) -> Text
renderActionCommit (action, commit) =
  "* [" <> messageHeadline commit <> "](" <> oid commit <> ")  *" <> actionToText action <> "*"

actionToText :: Action -> Text
actionToText action =
  case action of
    Reverted -> "Reverted"
    ResolvedConflicts -> "Resolved conflicts"
    SkippedMerge -> "Skipped merge commit"

-- HELPERS

inprocJSON :: Aeson.FromJSON a => Text -> [Text] -> Shell Line -> Shell a
inprocJSON command args stdin = do
  raw <- inproc command args stdin
  case Aeson.eitherDecodeStrict' (BS.fromString (Text.unpack (lineToText raw))) of
    Left err -> fail err
    Right parsed -> pure parsed

echoText :: Text -> Shell ()
echoText text =
  case textToLine text of
    Nothing -> pure ()
    Just line -> echo line

returnsList :: Shell a -> Shell [a]
returnsList shell =
  reduce (Fold (\acc x -> acc <> [x]) [] id) shell
