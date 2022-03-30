{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified App.Abutting as App (flatten)
import qualified App.Annotate as App (devDocs, userDocs)
import qualified App.Config as Config
  ( Config (..),
    Input (..),
    loadConfig,
    readConfig,
  )
import App.FixN (both, cataN)
import qualified App.Graph as Graph (alg, runGen)
import qualified App.Options as Opt (Options (..), Output (..), parseOptions)
import qualified App.Parse as App (Metadata (..), parse)
import qualified App.Render as App (render)
import qualified App.Tangle as Tangle (alg)
import App.Types (Tangle (..))
import Control.Monad (when, (>=>))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_, traverse_)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO (putStrLn, readFile)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import qualified System.Environment as Env
import System.FilePath (takeDirectory, (</>))
import qualified Text.Pandoc as PD
import Prelude hiding (readFile)

main :: IO ()
main = do
  options <- Opt.parseOptions <$> Env.getArgs
  case options of
    Left (errs, msg) -> mapM_ putStr errs >> putStr msg
    Right (Opt.Help msg) -> putStr msg
    Right (Opt.Run buildCfg outputs) -> do
      let alg = both Graph.alg Tangle.alg
      cfg <- Config.readConfig buildCfg >>= Config.loadConfig
      for_ (Config.config'inputs cfg) $ \(Config.Input inp outDev outUser) ->
        (runExceptT >=> either Text.IO.putStrLn pure) $ do
          (doc, metadata, roots) <- readFile inp >>= App.parse
          tmpl <- getTemplate cfg "default"
          let results = [(root, cataN alg fixn) | (root, fixn) <- roots]
              graph = Graph.runGen (traverse_ (fst . snd) results)
          when (Opt.OutputTangles `elem` outputs) $ do
            for_ results $ \(root, (_, tangled)) -> do
              let path = Config.config'tangleDir cfg </> tangle'path root
              wr path (App.flatten tangled)
          when (Opt.OutputDevDocs `elem` outputs) $ do
            doc' <- App.devDocs (takeDirectory inp) metadata graph doc
            html <- App.render tmpl (App.metadata'title metadata) doc'
            wr outDev html
          when (Opt.OutputUserDocs `elem` outputs) $ do
            doc' <- App.userDocs (takeDirectory inp) metadata graph doc
            html <- App.render tmpl (App.metadata'title metadata) doc'
            wr outUser html

--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . Text.IO.readFile

wr :: MonadIO m => FilePath -> LText.Text -> m ()
wr path ltxt = liftIO $ do
  createDirectoryIfMissing True (takeDirectory path)
  LText.IO.writeFile path ltxt

getTemplate ::
  MonadError Text m => Config.Config -> Text -> m (PD.Template LText.Text)
getTemplate cfg nm =
  maybe (throwError "default template not found") pure $
    Map.lookup nm (Config.config'templates cfg)
