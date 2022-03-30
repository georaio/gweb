{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Graph
  ( Graph,
    Edge (..),
    graph,
    toDot,

    -- * The Algebra
    alg,
    Gen,
    runGen,
  )
where

import App.FixN (AlgN, FixN, cataN)
import App.Parse (Literate (..))
import App.Types (BlockName (..), ParsedCode (..))
import Control.Applicative (liftA3)
import Control.Monad.State.Strict (State, execState, modify)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import qualified Data.Graph.Inductive as G
import Data.Monoid (First (First))
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import Text.Printf (printf)

type Graph = G.Gr (Text, Literate BlockName) Edge

data Edge = Link | Next
  deriving (Eq, Show)

newtype Gen a = Gen {runGenG :: State Graph a}
  deriving (Functor, Applicative, Monad)

instance Semigroup a => Semigroup (Gen a) where
  Gen m <> Gen n = Gen ((<>) <$> m <*> n)

instance Monoid a => Monoid (Gen a) where
  mempty = pure mempty
  mappend = (<>)

graph :: Foldable f => f (FixN Literate) -> Graph
graph = runGen . traverse_ (cataN alg)

runGen :: Gen a -> Graph
runGen = flip execState G.empty . runGenG

alg :: AlgN Literate (Gen (First (Int, Text)))
alg lits = fmap mconcat $
  for (zipPrev lits) $ \(Literate i attr@(name, _, _) code, prev) -> do
    (code', addEdges) <- fmap (unzip . concat) . for code $ \case
      Code t -> pure [(Code t, pure ())]
      Include n x -> do
        First r <- x
        pure
          [ (Include n (BlockName nm), addEdge Link (Just i) (Just j))
            | let Just (j, nm) = r
          ]
    addNode i (name, Literate i attr code')
    addEdge Next (litId <$> prev) (Just i)
    sequence_ addEdges
    pure (First (Just (i, name)))
  where
    addNode i x =
      Gen . modify $ liftA3 bool (G.insNode (i, x)) id (G.gelem i)
    addEdge e (Just i) (Just j) =
      Gen . modify $ liftA3 bool (G.insEdge (i, j, e)) id (`G.hasEdge` (i, j))
    addEdge _ _ _ = pure ()

-- | Zip the elements of a list with the previous element, or with 'Nothing' for
-- the first value.
zipPrev :: [a] -> [(a, Maybe a)]
zipPrev xs = zip xs (Nothing : map Just xs)

toDot :: Graph -> LText.Text
toDot gr =
  mconcat
    [ "digraph G {\n",
      LText.unlines nodes,
      LText.unlines edges,
      "}\n"
    ]
  where
    nodes =
      [ LText.pack (msg :: String)
        | (i, (nm, _)) <- G.labNodes gr,
          let msg =
                printf "  u%d [label=%s];" i (show nm :: String)
      ]
    edges =
      [ LText.pack (msg :: String)
        | (i, j, ty) <- G.labEdges gr,
          let msg =
                printf "  u%d -> u%d [label=\"%s\"];" i j (show ty :: String)
      ]
