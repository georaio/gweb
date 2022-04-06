{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App.Annotate (devDocs, userDocs) where

import App.Diagram (diagrams)
import App.Graph (Edge (..), Graph)
import App.Parse (Literate (..), Metadata (..))
import App.Types (BlockName (..), MapMonoid (..), ParsedCode (..), mapMonoid)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, modify, runStateT, state)
import Data.Bifunctor (bimap)
import Data.Foldable (fold, toList)
import qualified Data.Graph.Inductive as G
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Pandoc.Definition as PD
import Text.Pandoc.Walk (Walkable (query, walk, walkM))
import Text.Read (readMaybe)

devDocs ::
  (MonadError Text m, MonadIO m) =>
  (FilePath -> Metadata -> Graph -> PD.Pandoc -> m PD.Pandoc)
devDocs dir metadata gr doc = do
  let doc' = flip walk doc $ \case
        PD.Div (ident, cls, kv) body
          | "review-remark" `elem` cls ->
              PD.Div
                (ident, cls, kv)
                ( [ PD.Div ("", ["reviewer"], []) [PD.Plain [PD.Str ("-- " <> r)]]
                    | ("reviewer", r) <- kv
                  ]
                    ++ body
                )
        blk -> blk
  (docAnn, headers) <- runStateT (annotate' gr doc') initHeaders
  docToc <-
    if metadata'genToC metadata
      then pure (annotate'TableOfContents headers docAnn)
      else pure docAnn
  diagrams dir docToc

userDocs ::
  (MonadError Text m, MonadIO m) =>
  (FilePath -> Metadata -> Graph -> PD.Pandoc -> m PD.Pandoc)
userDocs dir metadata gr doc = do
  let doc' = flip walk doc $ \case
        PD.Div (_, cls, _) _
          | "dev-only" `elem` cls -> PD.Plain []
          | "review-remark" `elem` cls -> PD.Plain []
        blk -> blk
  (docAnn, headers) <- runStateT (annotate' gr doc') initHeaders
  docToc <-
    if metadata'genToC metadata
      then pure (annotate'TableOfContents headers docAnn)
      else pure docAnn
  diagrams dir docToc

annotate' ::
  (MonadError Text m, MonadState (Headers Int) m) =>
  (Graph -> PD.Pandoc -> m PD.Pandoc)
annotate' gr =
  walkM $ \case
    PD.CodeBlock attr x ->
      -- If this codeblock isn't a literate programming codeblock then leave it
      -- unchanged.
      fromMaybe (PD.CodeBlock (noLitId attr) x) <$> annotate'CodeBlock gr attr x
    PD.Header lvl (ident, cls, _kv) title -> do
      -- Collect all the headers to build the table of contents.
      hdr <- state (nextSection 1 succ ident cls lvl)
      modify (addHeader hdr title)
      pure (renderBodyHeader title hdr)
    -- PD.Div (ident, cls, kv) body
    --   | "review-remark" `elem` cls -> undefined
    blk -> pure blk
  where
    noLitId (blkId, cls, kv) =
      (blkId, cls, filter ((/= "literate-id") . fst) kv)

annotate'CodeBlock ::
  MonadError Text m => Graph -> PD.Attr -> Text -> m (Maybe PD.Block)
annotate'CodeBlock gr (blkId, cls, kv) _body = pure $ do
  ident <- List.lookup "literate-id" kv
  i <- readMaybe (Text.unpack ident)
  ctx <- fst (G.match i gr)
  Just . PD.Div ("src-" <> ident, "literate" : cls, []) $
    [ PD.Div ("", ["header"], []) $
        ( PD.Plain . concat $
            [ renderNeighbour ("pred", "<<", blkId) (G.inn', fst) ctx,
              renderNeighbour ("succ", ">>", blkId) (G.out', snd) ctx,
              [renderTitle blkId]
            ]
        ) :
        renderInclusions
          ( do
              -- Find the head of the chain, then find all the blocks that
              -- include it.
              let (nm, _) = G.lab' ctx
              hd <- toList (Map.lookup nm heads)
              j <- Map.findWithDefault [] hd incoming
              (j,) <$> toList (Map.lookup j names)
          ),
      renderCodeBlock heads (snd (G.lab' ctx))
    ]
  where
    (names, (incoming, _outgoing)) = allLinks gr
    heads = headNodes gr

annotate'TableOfContents ::
  Headers Int -> PD.Pandoc -> PD.Pandoc
annotate'TableOfContents headers (PD.Pandoc meta doc) =
  PD.Pandoc meta (toc ++ doc)
  where
    tocInfo = filter (trim 2) (headerAccum headers [])
    toc = [PD.Div ("table-of-contents", [], []) [PD.BulletList (map f tocInfo)]]
      where
        f (hdr, nm) =
          [ PD.Plain
              [ PD.Link
                  ("", [], [])
                  [renderToCHeader nm hdr]
                  ("#" <> target hdr, "")
              ]
          ]
    trim n = \case
      (Section _ path, _) -> length path <= n
      (Appendix _ path, _) -> length path <= n
    target = \case
      Section i _ -> i
      Appendix i _ -> i

renderBodyHeader :: [PD.Inline] -> Header Int -> PD.Block
renderBodyHeader title = \case
  Section ident path ->
    PD.Header
      (length path)
      (ident, [], [])
      ( PD.Span ("", ["section-no"], []) [PD.Str (Text.intercalate "." $ map (Text.pack . show) path)] :
        PD.Space :
        title
      )
  Appendix _ [] -> PD.Plain [PD.Str ""]
  Appendix ident (x : xs) ->
    PD.Header
      (length xs + 1)
      (ident, ["appendix"], [])
      ( PD.Str (if null xs then "Appendix " <> ts else ts) :
        PD.Str ":" :
        PD.Space :
        title
      )
    where
      ts =
        Text.intercalate "." $
          Text.singleton (['A' ..] List.!! (x - 1)) : map (Text.pack . show) xs

renderToCHeader :: Text -> Header Int -> PD.Inline
renderToCHeader title = \case
  Section _ path ->
    PD.Span
      ("", ["section"], [("toc-level", Text.pack (show (length path)))])
      [ PD.Span ("", ["toc-number"], []) [PD.Str (Text.intercalate "." $ map (Text.pack . show) path)],
        PD.Span ("", ["toc-item"], []) [PD.Str title]
      ]
  Appendix _ [] -> PD.Str ""
  Appendix _ path@(x : xs) ->
    PD.Span
      ("", ["appendix"], [("toc-level", if null xs then "Appendix " <> ts else ts)])
      [ PD.Span ("", ["toc-number"], []) [PD.Str (Text.intercalate "." $ map (Text.pack . show) path)],
        PD.Span ("", ["toc-item"], []) [PD.Str title]
      ]
    where
      ts =
        Text.intercalate "." $
          Text.singleton (['A' ..] List.!! (x - 1)) : map (Text.pack . show) xs

renderTitle :: Text -> PD.Inline
renderTitle blkId = PD.Span ("", ["title"], []) [PD.Str blkId]

renderNeighbour ::
  Show a =>
  (Text, Text, Text) ->
  (G.Context node Edge -> [(a, a, Edge)], (a, a) -> a) ->
  (G.Context node Edge -> [PD.Inline])
renderNeighbour (lbl, txt, blkId) (edges, prj) ctx =
  case prj3 <$> List.find (\(_, _, x) -> x == Next) (edges ctx) of
    Nothing ->
      [ PD.Span ("", [lbl], []) [PD.Str txt]
      ]
    Just j ->
      [ PD.Span
          ("", [lbl], [])
          [ PD.Link
              ("", [], [])
              [PD.Str txt]
              ( "#src-" <> Text.pack (show j),
                blkId <> ":" <> Text.pack (show j)
              )
          ]
      ]
  where
    prj3 (i, j, _) = prj (i, j)

renderInclusions :: [(G.Node, Text)] -> [PD.Block]
renderInclusions incls =
  [ PD.Plain [PD.Str " | ", PD.Span ("", ["inclusions"], []) items]
    | not (null items)
  ]
  where
    items =
      List.intersperse PD.Space $
        [ PD.Link
            ("", [], [])
            [PD.Str lbl]
            ("#src-" <> Text.pack (show j), lbl <> ":" <> Text.pack (show j))
          | (j, lbl) <- List.sort incls
        ]

renderCodeBlock :: Map Text G.Node -> Literate BlockName -> PD.Block
renderCodeBlock heads lit =
  PD.Div ("", ["src"], []) . (: []) . PD.Plain . concat $
    [ case block of
        Code t -> [PD.Str t]
        Include _ (BlockName nm) ->
          case Map.lookup nm heads of
            Nothing ->
              [ PD.Str "<<",
                PD.Link ("", [], []) [PD.Str nm] ("#", ""),
                PD.Str ">>"
              ]
            Just j ->
              [ PD.Str "<<",
                PD.Link
                  ("", [], [])
                  [PD.Str nm]
                  ("#src-" <> Text.pack (show j), ""),
                PD.Str ">>"
              ]
      | block <- litCode lit
    ]

--
-- HEADERS
--

data HeaderState a
  = InSection [a] [a]
  | InAppendix [a] [a]
  deriving (Show)

data Header a
  = Section Text [a]
  | Appendix Text [a]
  deriving (Show)

data Headers a = Headers
  { headerState :: HeaderState a,
    headerAccum :: [(Header a, Text)] -> [(Header a, Text)]
  }

initHeaders :: Headers a
initHeaders = Headers (InSection [] []) id

nextSection ::
  a -> (a -> a) -> Text -> [Text] -> Int -> Headers a -> (Header a, Headers a)
nextSection initA nextA ident classes lvl hdrs =
  (path, hdrs {headerState = st''})
  where
    st'
      | lvl == 1 = setState paths
      | otherwise = headerState hdrs
    (path, st'') = updateSections (incrHeader initA nextA lvl) st'
    paths = case headerState hdrs of
      InSection s a -> (s, a)
      InAppendix s a -> (s, a)
    setState (s, a)
      | "appendix" `elem` classes = InAppendix s a
      | otherwise = InSection s a
    updateSections :: ([a] -> [a]) -> HeaderState a -> (Header a, HeaderState a)
    updateSections f = \case
      InSection s a -> (Section ident (reverse (f s)), InSection (f s) a)
      InAppendix s a -> (Appendix ident (reverse (f a)), InAppendix s (f a))

addHeader :: Header a -> [PD.Inline] -> Headers a -> Headers a
addHeader hdr nm hdrs =
  hdrs
    { headerAccum = headerAccum hdrs . ((hdr, extractText nm) :)
    }

incrHeader :: a -> (a -> a) -> Int -> [a] -> [a]
incrHeader initA nextA lvl hdr =
  case compare lvl' lvl of
    LT -> replicate (lvl - lvl') initA ++ hdr
    EQ -> incr hdr
    GT -> incr (drop (lvl' - lvl) hdr)
  where
    lvl' = length hdr
    incr = \case
      [] -> []
      x : xs -> nextA x : xs

--
-- HELPERS
--

extractText :: Walkable PD.Inline a => a -> Text
extractText = query $ \case
  PD.Str x -> x
  PD.Space -> " "
  _ -> ""

-- Find the nodes that are at the head of each named chain of blocks.
headNodes :: Graph -> Map Text G.Node
headNodes gr =
  Map.fromList
    [ (nm, i)
      | (i, (nm, _)) <- G.labNodes gr,
        null [() | (_, _, Next) <- G.inn gr i]
    ]

-- Collect all the incoming and outgoing inclusion links between nodes.
allLinks ::
  Graph -> (Map G.Node Text, (Map G.Node [G.Node], Map G.Node [G.Node]))
allLinks = fmap (bimap unMapMonoid unMapMonoid) . G.ufold fn mempty
  where
    fn ctx m =
      fold
        ( m :
          (Map.singleton i lbl, mempty) :
            [ (mempty, (mapMonoid j [k], mapMonoid k [j]))
              | (j, k) <- inn ++ out
            ]
        )
      where
        (i, (lbl, _)) = G.labNode' ctx
        inn = [(i, j) | (j, _, Link) <- G.inn' ctx]
        out = [(j, i) | (_, j, Link) <- G.out' ctx]
