{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Render (render) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Text.DocLayout as Doc
import qualified Text.Pandoc as PD

render ::
  (MonadError Text m, MonadIO m) =>
  (PD.Template LText.Text -> Text -> PD.Pandoc -> m LText.Text)
render tmpl title doc = do
  html <-
    either (throwError . Text.pack . show) pure $
      PD.runPure (PD.writeHtml4 htmlOpts doc)
  pure $ Doc.render Nothing $ PD.renderTemplate tmpl (context html)
  where
    htmlOpts = PD.def {PD.writerWrapText = PD.WrapNone}
    context :: Blaze.Markup -> Map Text LText.Text
    context html =
      Map.fromList
        [ ("title", LText.fromStrict title),
          ("body", Blaze.renderMarkup html)
        ]
