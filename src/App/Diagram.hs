{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Diagram (diagrams) where

import qualified Control.Lens as L
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (MonadState, modify, runStateT)
import Control.Monad.Writer (runWriterT, tell)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified System.Process as Proc
import qualified Text.Pandoc.Definition as PD
import Text.Pandoc.Walk (Walkable (walkM))
import Text.Printf (printf)
import qualified Text.XML.Light as XML
import qualified Text.XML.Light.Lens as XML.L
import Validation (Validation (Failure, Success))

diagrams ::
  (MonadIO m) =>
  (FilePath -> PD.Pandoc -> m PD.Pandoc)
diagrams dir doc = do
  (doc', tables) <- runStateT (findGnuplotDataTables doc) []
  flip walkM doc' $ \case
    PD.CodeBlock attr@(_, cls, _) body
      | "pikchr" `elem` cls -> pikchr attr body
      | "dot" `elem` cls -> graphviz attr body
      | "gnuplot" `elem` cls -> gnuplot dir tables attr body
    blk -> pure blk

--------------------------------------------------------------------------------
-- PIKCHR ----------------------------------------------------------------------
--------------------------------------------------------------------------------

pikchr :: MonadIO m => PD.Attr -> Text -> m PD.Block
pikchr attr body = do
  (ok, stdout, _stderr) <-
    liftIO $
      Proc.readCreateProcessWithExitCode
        (Proc.proc "pikchr" ["--svg-only", "-"])
        (Text.unpack body)
  case ok of
    ExitSuccess ->
      pure $ PD.Div attr [PD.RawBlock (PD.Format "html") (Text.pack stdout)]
    ExitFailure _errCode ->
      pure . PD.Div ("", [], []) $
        [ PD.CodeBlock attr body,
          PD.CodeBlock ("", ["diagram-error"], []) (Text.pack stdout)
        ]

--------------------------------------------------------------------------------
-- GRAPHVIZ --------------------------------------------------------------------
--------------------------------------------------------------------------------

graphviz :: MonadIO m => PD.Attr -> Text -> m PD.Block
graphviz attr body = do
  (ok, stdout, stderr) <-
    liftIO $
      Proc.readCreateProcessWithExitCode
        (Proc.proc "dot" ["-Tsvg"])
        (Text.unpack body)
  case ok of
    ExitSuccess ->
      pure $ PD.Div attr [PD.RawBlock (PD.Format "html") (Text.pack stdout)]
    ExitFailure _errCode ->
      pure . PD.Div ("", [], []) $
        [ PD.CodeBlock attr body,
          PD.CodeBlock ("", ["diagram-error"], []) (Text.pack stderr)
        ]

--------------------------------------------------------------------------------
-- GNUPLOT ---------------------------------------------------------------------
--------------------------------------------------------------------------------

type GnuplotTables = [(Text, [[Text]])]

data GnuplotDataSourceRef
  = FileDataSource Text Text
  | TableDataSource Text

shortcircuiting :: Monad m => ExceptT a m a -> m a
shortcircuiting = runExceptT >=> either pure pure

finish :: Monad m => a -> ExceptT a m b
finish = throwError

gnuplot ::
  MonadIO m =>
  (FilePath -> GnuplotTables -> PD.Attr -> Text -> m PD.Block)
gnuplot dir tables attr@(ident, cls, kw) body = shortcircuiting $ do
  let tableNames :: [GnuplotDataSourceRef]
      tableNames =
        [ FileDataSource (Text.drop 5 k) path
          | (k, path) <- kw,
            "file-" `Text.isPrefixOf` k
        ]
          ++ [TableDataSource nm | ("table", nm) <- kw]

  dataBlocks <- case traverse mkDataBlock tableNames of
    Failure errors ->
      finish $
        PD.Div
          (ident, cls, kw)
          [ PD.CodeBlock ("", [], []) body,
            PD.Div
              ("", ["diagram-error"], [])
              [ PD.OrderedList
                  (1, PD.DefaultStyle, PD.DefaultDelim)
                  [[PD.Plain [PD.Str err]] | err <- errors]
              ]
          ]
    Success dbs -> pure (concat dbs)
  let prelude = "set terminal svg\nset output\n"
  (ok, stdout, stderr) <-
    liftIO . Proc.readCreateProcessWithExitCode (Proc.proc "gnuplot" []) $
      prelude ++ dataBlocks ++ Text.unpack body
  finish $ case ok of
    ExitSuccess -> case XML.parseXMLDoc stdout of
      Nothing ->
        PD.Div
          ("", [], [])
          [ PD.CodeBlock attr body,
            PD.CodeBlock ("", ["diagram-error"], []) "Couldn't parse SVG"
          ]
      Just svg ->
        PD.Div
          attr
          [ PD.RawBlock
              (PD.Format "html")
              (Text.pack (XML.showTopElement (stripDesc svg)))
          ]
    ExitFailure _errCode ->
      PD.Div
        ("", [], [])
        [ PD.CodeBlock attr body,
          PD.CodeBlock ("", ["diagram-error"], []) (Text.pack stderr)
        ]
  where
    mkDataBlock :: GnuplotDataSourceRef -> Validation [Text] String
    mkDataBlock = \case
      FileDataSource nm path ->
        pure $
          Text.unpack nm ++ " = " ++ show (dir </> Text.unpack path) ++ "\n"
      TableDataSource nm ->
        let fmt = concatMap ((++ "\n") . unwords . map Text.unpack)
         in case List.lookup nm tables of
              Nothing ->
                Failure
                  [ Text.pack . printf "Referenced data table not found: %s" $
                      (show nm :: String)
                  ]
              Just tbl ->
                pure $
                  "$" ++ Text.unpack nm ++ " << EOD\n" ++ fmt tbl ++ "EOD\n"

findGnuplotDataTables ::
  MonadState GnuplotTables m => PD.Pandoc -> m PD.Pandoc
findGnuplotDataTables = walkM $ \case
  PD.Div attrs@(_, cls, kw) body
    | Just nm <- List.lookup "data-table" kw -> do
      (blk', errors) <- runWriterT $ case body of
        [PD.Table _ _ _ _ tableBodies _] -> do
          rows <-
            for tableBodies $ \(PD.TableBody _ _ _ rows) ->
              for rows $ \(PD.Row _ cells) ->
                for cells $ \(PD.Cell _ _ _ _ bs) ->
                  plain nm bs
          modify ((nm, concat rows) :)
          pure (if "hidden" `elem` cls then PD.Null else PD.Div attrs body)
        _ ->
          PD.Div attrs body
            <$ tell ["Bad data-table, expected a table inside a data-table div"]
      case errors of
        [] -> pure blk'
        _ ->
          pure . PD.Div attrs . (body ++) $
            [ PD.Div
                ("", ["diagram-error"], [])
                [ PD.OrderedList (1, PD.DefaultStyle, PD.DefaultDelim) $
                    [[PD.Plain [PD.Str err]] | err <- errors]
                ]
            ]
  blk -> pure blk
  where
    plain nm bs
      | [PD.Plain [PD.Str x]] <- bs = pure x
      | otherwise =
        let msg = "Bad data table \"%s\": failed to parse plain text from %s"
         in "" <$ tell [Text.pack (printf msg nm (show bs :: String))]

-- parseGnuplotTableBodies ::
--   Monad m => Text -> [PD.TableBody] -> WriterT [Text] m GnuplotTables
-- parseGnuplotTableBodies nm tableBodies =

-- | Remove the <desc>...</desc> element which says what version of Gnuplot
-- generated the plot. This just makes golden tests fail for no good reason.
stripDesc :: XML.Element -> XML.Element
stripDesc = L.over XML.L.elContentL (L.toListOf (L.folded . L.filtered isDescQ))
  where
    isDescQ :: XML.Content -> Bool
    isDescQ cnt = case cnt of
      XML.Elem el -> XML.elName el /= descQ
      _ -> True
    descQ = XML.QName "desc" (Just "http://www.w3.org/2000/svg") Nothing
