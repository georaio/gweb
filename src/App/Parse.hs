{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module App.Parse (Literate (..), Metadata (..), parse) where

import App.FixN (FixN (..))
import App.Types
  ( BlockName (..),
    ParsedCode (..),
    Tangle (..),
    shows3With,
  )
import Control.Applicative (liftA3)
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (get, put, runState)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Functor.Classes (Show1 (..))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import GHC.Generics (Generic)
import Quiet (Quiet (..))
import qualified Text.Pandoc as PD
import Text.Pandoc.Walk (Walkable (query, walkM))
import Text.Printf (printf)
import Validation (Validation (Failure, Success), validation)

-- | A mapping from block names to parsed code blocks which we get from parsing
-- a Pandoc document.
type CodeBlocks =
  Map BlockName (NonEmpty (Literate BlockName))

data Literate a = Literate
  { litId :: Int,
    litAttr :: PD.Attr,
    litCode :: [ParsedCode Text a]
  }
  deriving (Functor, Generic)
  deriving (Show, Read) via (Quiet (Literate a))

litBlockName :: Literate a -> BlockName
litBlockName (Literate _ (k, _, _) _) = BlockName k

instance Show1 Literate where
  liftShowsPrec sp sl d (Literate i attr cb) =
    shows3With sI sAttr sCB "Literate" d i attr cb
    where
      sI = showsPrec
      sAttr = showsPrec
      sCB = \_ -> liftShowList sp sl

throwShow ::
  (MonadError Text m, Show a) =>
  ((a -> m b) -> (b -> m b) -> e -> m b) ->
  (e -> m b)
throwShow f = f (throwError . Text.pack . show) pure

parse ::
  MonadError Text m =>
  (Text -> m (PD.Pandoc, Metadata, [(Tangle, FixN Literate)]))
parse =
  throwShow either . PD.runPure . PD.readMarkdown mdOpts
    >=> throwShow validation . parsePandoc
  where
    mdOpts = PD.def {PD.readerExtensions = PD.pandocExtensions}

parsePandoc ::
  PD.Pandoc ->
  Validation [Text] (PD.Pandoc, Metadata, [(Tangle, FixN Literate)])
parsePandoc doc =
  case extractInfo doc of
    Failure errs -> Failure errs
    Success (doc', metadata@(Metadata _ _ roots), blocks) ->
      fmap (doc',metadata,) . for (toList roots) $ \root ->
        fmap (root,) . fixed blocks $ root

extractInfo ::
  PD.Pandoc -> Validation [Text] (PD.Pandoc, Metadata, CodeBlocks)
extractInfo doc@(PD.Pandoc meta _) =
  (doc',,groupBy litBlockName (allCode [])) <$> parseMetadata meta
  where
    (doc', (_, allCode)) =
      flip runState (0 :: Int, id) . flip walkM doc $ \case
        PD.CodeBlock attr@(blkId, cls, kv) body | blkId /= "" -> do
          (i, acc) <- get
          let kv' = kv ++ [("literate-id", Text.pack (show i))]
          put (succ i, (Literate i attr (parseCodeBlock body) :) . acc)
          pure (PD.CodeBlock (blkId, cls, kv') body)
        blk ->
          pure blk

fixed :: CodeBlocks -> Tangle -> Validation [Text] (FixN Literate)
fixed blocks root = go Set.empty (BlockName (tangle'name root))
  where
    go seen b@(BlockName nm) =
      case (Set.member b seen, Map.lookup b blocks) of
        (_, Nothing) ->
          Failure [nm <> " not found"]
        (True, _) ->
          Failure [nm <> " is in an infinite loop"]
        (False, Just xs) ->
          fmap FixN . for (toList xs) $ \(Literate k attr bs) ->
            fmap (Literate k attr) . for bs $ \case
              Code t -> pure (Code t)
              Include ind b' -> Include ind <$> go (Set.insert b seen) b'

--
-- PARSING METADATA
--

data Metadata = Metadata
  { metadata'title :: Text,
    metadata'genToC :: Bool,
    metadata'tangles :: Set Tangle
  }
  deriving (Show)

parseMetadata :: PD.Meta -> Validation [Text] Metadata
parseMetadata (PD.Meta meta) = liftA3 Metadata titleV genToCV tanglesV
  where
    q :: Text -> Map Text PD.MetaValue -> Validation [Text] Text
    q k = maybe (Failure [err k]) (pure . extractText) . Map.lookup k
    err k = "Extracting tangle roots: field not found: " <> Text.pack (show k)
    titleV =
      case Map.lookup "title" meta of
        Nothing -> Failure ["No title for document"]
        Just x -> pure $ extractText x
    genToCV =
      case Map.lookup "generate-toc" meta of
        Nothing -> pure False
        Just (PD.MetaBool b) -> pure b
        Just x ->
          Failure
            [ Text.pack $
                printf @(String -> String)
                  "Expected boolean for generate-toc (%s)"
                  (show x)
            ]
    tanglesV =
      case Map.lookup "tangles" meta of
        Nothing -> mempty
        Just (PD.MetaList ts) -> flip foldMap ts $ \case
          PD.MetaMap m ->
            (\nm path lang -> Set.singleton (Tangle nm (Text.unpack path) lang))
              <$> q "name" m <*> q "path" m <*> q "language" m
          _ -> Failure ["Failed to parse tangle roots"]
        Just _ -> Failure ["Failed to parse tangle roots"]

extractText :: Walkable PD.Inline a => a -> Text
extractText = query $ \case
  PD.Str x -> x
  PD.Space -> " "
  _ -> ""

--
-- PARSING CODE BLOCKS
--

parseCodeBlock :: Text -> [ParsedCode Text BlockName]
parseCodeBlock = go []
  where
    go acc t =
      case Text.breakOn "<<" t of
        ("", "") -> case acc of
          [] -> []
          _ -> [Code (finish acc "")]
        (c, "") -> [Code (finish acc c)]
        (c, t') -> case Text.break (not . isBlockName) (Text.drop 2 t') of
          (_, "") -> [Code (finish acc t)]
          (bname, t'') -> case Text.splitAt 2 t'' of
            (">>", t''')
              | not (Text.null bname) ->
                Code (finish acc c) :
                Include (indent c) (BlockName bname) :
                go [] t'''
            (_, _) ->
              let len = Text.length c + Text.length bname + 2
               in go (Text.take len t : acc) t''
    isBlockName c = isAlphaNum c || c `elem` ['-', '_']
    indent c = Text.length (Text.takeWhileEnd (/= '\n') c)
    finish acc t = List.foldr1 (flip (<>)) (t : acc)

groupBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f = foldr fn Map.empty
  where
    fn x m =
      let k = f x
       in case Map.lookup k m of
            Nothing -> Map.insert k (x :| []) m
            Just xs -> Map.insert k (xs <> (x :| [])) m
