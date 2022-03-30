{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Golden.Test (tests) where

import qualified App.Annotate as SUT (devDocs, userDocs)
import qualified App.Graph as SUT (graph, toDot)
import qualified App.Parse as SUT (Metadata (..), parse)
import qualified App.Render as SUT (render)
import qualified App.Tangle as SUT (tangle)
import App.Types (Tangle (..))
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import qualified System.FilePath as Path
import System.IO (Handle, IOMode (WriteMode), hPutStr, hPutStrLn, withFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFileDiff)
import qualified Text.Pandoc as PD
import Text.Printf (printf)
import qualified Text.Show.Pretty as PP

tests :: IO [TestTree]
tests =
  sequence
    [ findGoldens "test-golden" ".md" $ \k ->
        sequence
          [ k "parsed" $ \_path md h -> runE h $ do
              (doc, metadata, tangles) <- SUT.parse md
              prn h $ printf "title: %s\n========" (SUT.metadata'title metadata)
              pretty h tangles
              prn h "========"
              pretty h doc,
            k "tangled" $ \_path md h -> runE h $ do
              (_, _, tangles) <- SUT.parse md
              for_ tangles $ \(Tangle name path lang, t) -> do
                prn h $ printf "=== %s [%s/%s] ===" path name lang
                pr h $ LText.unpack (SUT.tangle t),
            k "graph" $ \_path md h -> runE h $ do
              (_, _, tangles) <- SUT.parse md
              pr h $ LText.unpack $ SUT.toDot $ SUT.graph (map snd tangles),
            k "pandoc-dev" $ \path md h -> runE h $ do
              let dir = Path.takeDirectory path
              (doc, metadata, tangles) <- SUT.parse md
              doc' <- SUT.devDocs dir metadata (SUT.graph (map snd tangles)) doc
              pretty h doc',
            k "pandoc-user" $ \path md h -> runE h $ do
              let dir = Path.takeDirectory path
              (doc, metadata, tangles) <- SUT.parse md
              doc' <- SUT.userDocs dir metadata (SUT.graph (map snd tangles)) doc
              pretty h doc',
            k "html-dev" $ \path md h -> runE h $ do
              let dir = Path.takeDirectory path
              (doc, metadata, tangles) <- SUT.parse md
              doc' <- SUT.devDocs dir metadata (SUT.graph (map snd tangles)) doc
              tmpl <- getTemplate
              html <- SUT.render tmpl (SUT.metadata'title metadata) doc'
              pr h (LText.unpack html),
            k "html-user" $ \path md h -> runE h $ do
              let dir = Path.takeDirectory path
              (doc, metadata, tangles) <- SUT.parse md
              doc' <- SUT.userDocs dir metadata (SUT.graph (map snd tangles)) doc
              tmpl <- getTemplate
              html <- SUT.render tmpl (SUT.metadata'title metadata) doc'
              pr h (LText.unpack html)
          ]
    ]

--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------

prn :: (MonadIO m) => Handle -> String -> m ()
prn h = liftIO . hPutStrLn h

pr :: (MonadIO m) => Handle -> String -> m ()
pr h = liftIO . hPutStr h

pretty :: (Show a, MonadIO m) => Handle -> a -> m ()
pretty h = liftIO . hPutStrLn h . PP.ppShow

type GoldenCallback = FilePath -> Text -> Handle -> IO ()

findGoldens ::
  FilePath ->
  FilePath ->
  ((String -> GoldenCallback -> IO TestTree) -> IO [TestTree]) ->
  IO TestTree
findGoldens dir extnIn f = do
  let basename = Path.takeBaseName dir
  inputs <- List.sort <$> findByExtension [extnIn] dir
  fmap (testGroup basename) . for inputs $ \inp -> do
    let filename = Path.makeRelative dir inp
    let testGroupName = printf "%s" (Path.dropExtensions filename) :: String
    md <- Text.IO.readFile inp
    fmap (testGroup testGroupName) . f $ \extnGolden g -> do
      let golden = inp `Path.replaceExtension` extnGolden
          diff ref _new = ["git", "diff", "--exit-code", "-u", ref]
      pure $
        goldenVsFileDiff extnGolden diff golden golden $
          withFile golden WriteMode (g inp md)

runE :: MonadIO m => Handle -> ExceptT Text m () -> m ()
runE h = runExceptT >=> either (liftIO . Text.IO.hPutStrLn h) pure

getTemplate :: (MonadError Text m) => m (PD.Template LText.Text)
getTemplate =
  either (throwError . Text.pack) pure . runIdentity $
    PD.compileTemplate
      ""
      "<html><head><title>$title$</title></head>\n\
      \<body>\n$body$\n</body>\n\
      \</html>\n"
