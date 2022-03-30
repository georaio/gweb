{-# LANGUAGE LambdaCase #-}

module App.Options (Options (..), Output (..), parseOptions) where

import qualified Data.List as List
import Data.Maybe (fromMaybe, listToMaybe)
import qualified System.Console.GetOpt as GetOpt

data Opt
  = Opt'ConfigPath FilePath
  | Opt'OutputTangles
  | Opt'OutputUserDocs
  | Opt'OutputDevDocs
  | Opt'Help
  deriving (Eq, Show)

data Options = Help String | Run FilePath [Output]
  deriving (Show)

data Output = OutputTangles | OutputUserDocs | OutputDevDocs
  deriving (Eq, Show)

options :: [GetOpt.OptDescr Opt]
options =
  [ GetOpt.Option
      ['c']
      ["config"]
      (GetOpt.ReqArg Opt'ConfigPath "CONFIG_FILE")
      "Build configuration file.",
    GetOpt.Option
      ['T']
      ["tangle"]
      (GetOpt.NoArg Opt'OutputTangles)
      "Output tangled files.",
    GetOpt.Option
      ['U']
      ["user-docs"]
      (GetOpt.NoArg Opt'OutputUserDocs)
      "Output user documentation.",
    GetOpt.Option
      ['D']
      ["dev-docs"]
      (GetOpt.NoArg Opt'OutputDevDocs)
      "Output developer documentation.",
    GetOpt.Option
      "h?"
      ["help"]
      (GetOpt.NoArg Opt'Help)
      "Display this help and exit."
  ]

parseOptions :: [String] -> Either ([String], String) Options
parseOptions args =
  case errs of
    _ : _ -> Left (errs, GetOpt.usageInfo "gweb" options)
    []
      | Opt'Help `elem` opts -> Right (Help (GetOpt.usageInfo "gweb" options))
      | otherwise -> Right (Run buildCfg outputs)
  where
    (opts, _nonOpts, errs) =
      GetOpt.getOpt GetOpt.Permute options args
    buildCfg =
      fromMaybe "build.cfg" $
        listToMaybe [path | Opt'ConfigPath path <- opts]
    outputs =
      concatMap fn $ listOr allOutputs $ List.intersect allOutputs opts
    allOutputs =
      [Opt'OutputTangles, Opt'OutputUserDocs, Opt'OutputDevDocs]
    listOr xs = \case [] -> xs; ys -> ys
    fn = \case
      Opt'OutputTangles -> [OutputTangles]
      Opt'OutputUserDocs -> [OutputUserDocs]
      Opt'OutputDevDocs -> [OutputDevDocs]
      _ -> []
