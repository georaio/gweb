{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}

module App.Types where

import Data.Functor.Classes (Show1 (..), showsBinaryWith)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Text (Text)

-- | A description of source that needs to be tangled. 'tangle'name' must be a
-- code block name.
data Tangle = Tangle
  { tangle'name :: Text,
    tangle'path :: FilePath,
    tangle'language :: Text
  }
  deriving (Eq, Ord, Show, Read)

-- | A wrapper for code block names.
newtype BlockName = BlockName Text
  deriving (Eq, Ord, Show, Read)

-- | Either code text or an include reference. Polymorphic in the text type
-- ('t') and the reference values ('a').
data ParsedCode t a
  = Code t
  | Include Int a
  deriving (Eq, Show, Read, Functor, Foldable)

instance Show t => Show1 (ParsedCode t) where
  liftShowsPrec _ _ d (Code t) =
    showString "Code " . showsPrec d t
  liftShowsPrec sp _ d (Include i x) =
    showsBinaryWith showsPrec sp "Include" d i x

shows3With ::
  (Int -> a -> ShowS) ->
  (Int -> b -> ShowS) ->
  (Int -> c -> ShowS) ->
  (String -> Int -> a -> b -> c -> ShowS)
shows3With sp1 sp2 sp3 name d x y z =
  showParen (d > 10) $
    showString name
      . (showChar ' ' . sp1 11 x)
      . (showChar ' ' . sp2 11 y)
      . (showChar ' ' . sp3 11 z)

newtype MapMonoid k v = MapMonoid {unMapMonoid :: Map k v} deriving (Show)

mapMonoid :: k -> v -> MapMonoid k v
mapMonoid k v = MapMonoid (Map.singleton k v)

instance (Ord k, Semigroup v) => Semigroup (MapMonoid k v) where
  MapMonoid x <> MapMonoid y = MapMonoid (Map.merge f g h x y)
    where
      f = Map.mapMissing (const id)
      g = Map.mapMissing (const id)
      h = Map.zipWithAMatched (\_ a b -> pure (a <> b))

instance (Ord k, Monoid v) => Monoid (MapMonoid k v) where
  mempty = MapMonoid mempty
  mappend = (<>)
