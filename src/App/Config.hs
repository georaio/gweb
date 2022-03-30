{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config
  ( ConfigRead (..),
    Config (..),
    Input (..),
    readConfig,
    loadConfig,
  )
where

import Control.Applicative (some, (<|>))
import Data.Char (isAlpha, isUpper)
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO (readFile)
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import Data.Void (Void)
import System.Directory (listDirectory)
import System.FilePath
  ( dropExtension,
    replaceExtension,
    takeDirectory,
    takeExtension,
    (</>),
  )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Pandoc.Templates as PD
import Validation (Validation (Failure, Success))

data ConfigRead = ConfigRead
  { cr'docsDir :: FilePath,
    cr'templatesDir :: FilePath,
    cr'devOutputDir :: FilePath,
    cr'userOutputDir :: FilePath,
    cr'tangleDir :: FilePath
  }
  deriving (Show)

data Config = Config
  { config'inputs :: [Input],
    config'templates :: Map Text (PD.Template LText.Text),
    config'devOutputDir :: FilePath,
    config'userOutputDir :: FilePath,
    config'tangleDir :: FilePath
  }
  deriving (Show)

data Input = Input
  { input'inPath :: FilePath,
    input'outPathDev :: FilePath,
    input'outPathUser :: FilePath
  }
  deriving (Show)

readConfig :: FilePath -> IO ConfigRead
readConfig path = do
  let dir = takeDirectory path
  t <- Text.IO.readFile path
  case P.parse (parseConfigRead dir) path t of
    Left err -> error (show err)
    Right cfg -> pure cfg

loadConfig :: ConfigRead -> IO Config
loadConfig (ConfigRead docsDir tmplDir devOutDir userOutDir tnglDir) = do
  (inputs, _inputsOther) <-
    partition ((== ".md") . takeExtension) <$> listDirectory docsDir
  let inputOutputs =
        [ Input
            (docsDir </> x)
            (devOutDir </> replaceExtension x "html")
            (userOutDir </> replaceExtension x "html")
          | x <- inputs
        ]
  (tmpls, _tmplsOther) <-
    partition ((== ".template") . takeExtension) <$> listDirectory tmplDir
  compiledTmpls <- fmap Map.fromList . for tmpls $ \name -> do
    t <- Text.IO.readFile (tmplDir </> name)
    PD.compileTemplate "" t >>= \case
      Right tmpl -> pure (Text.pack (dropExtension name), tmpl)
      Left errs -> error (show errs)
  pure (Config inputOutputs compiledTmpls devOutDir userOutDir tnglDir)

parseConfigRead :: FilePath -> P.Parsec Void Text ConfigRead
parseConfigRead dir = do
  opts <- some parseOption
  let lookupV k =
        case lookup k opts of
          Nothing -> Failure [k <> " not found"]
          Just x -> pure x
  let cfgV =
        ConfigRead
          <$> ((dir </>) <$> lookupV "DOCS_PATH")
          <*> ((dir </>) <$> lookupV "TEMPLATES_PATH")
          <*> ((dir </>) <$> lookupV "DEV_OUTPUT_PATH")
          <*> ((dir </>) <$> lookupV "USER_OUTPUT_PATH")
          <*> ((dir </>) <$> lookupV "TANGLE_PATH")
  case cfgV of
    Failure errs -> error (show errs)
    Success cfg -> pure cfg

parseOption :: P.Parsec Void Text (Text, FilePath)
parseOption = do
  xs <- option
  spaces *> P.single '=' <* spaces
  ys <- filepath <* (() <$ P.eol <|> P.eof)
  pure (xs, Text.unpack ys)
  where
    scanner nm = P.label nm . P.takeWhile1P Nothing
    option = scanner "OPTION" (\x -> isUpper x || x == '_')
    filepath = scanner "FILEPATH" (\x -> isAlpha x || x `elem` ['_', '-', '/'])
    spaces = P.skipMany (P.single ' ')
