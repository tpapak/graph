{-|
Module      : Data.AdjacencyList.Graph
Description : Class definitions of the Graph

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Basic definitions of a graph as an adjacency list.
The graph is represented as the function that outputs 
a list of the adjacent vertices of a given vertex,
which is the function equivalent of the adjacency list.
 -}

{-# LANGUAGE DeriveGeneric #-}  

module Data.Graph.AdjacencyList
    ( Vertex (..)
    , Edge (..)
    , Neighbors (..)
    , EdgeMap (..)
    -- * Graph definition
    , Graph (..)
    , fromTuple
    , toTuple
    -- * createGraph: Graph constructor
    , createGraph
    -- * graph from list of Edges
    , graphFromEdges
    , edges
    , reverseEdge
    , reverseEdges
    , reverseGraph
    -- * filterVertices
    , filterVertices
    -- * filterEdges
    , filterEdges
    -- * makeUndirected
    , makeUndirected
    , adjacentEdges
    , edgesFromNeighbors
    , adjacencyMap
    , edgeExists
    , edgeIndex
    , from
    , to
    , numVertices
    , numEdges
    , removeReverseEdges
    , completeGraph
    ) where

import Data.List
import Data.List.Unique
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as Set
import qualified GHC.Generics as Gen
import qualified Data.Binary as Bin

type Vertex = Int

data Edge = Edge Vertex Vertex 
  deriving (Ord, Gen.Generic)
instance Bin.Binary Edge

instance Show Edge where
 show (Edge s t) = "[" ++ show s ++ "->" ++ show t ++ "]"

instance Eq Edge where
  a == b = from a == from b && to a == to b

type EdgeMap = M.Map Edge Int

-- | Takes vertex and outputs neighboring vertices.
-- The Neighbors type is the function from a vertex to its neighbors
type Neighbors = (Vertex -> [Vertex])

-- | Graph definition of directed Graphs 
-- undirected graphs should include reverse edges.
data Graph = 
  Graph { vertices :: [Vertex] -- ^ The domain of the `neighbors` function. 
        -- It is usefull for finite graphs.
        , edgeMap :: EdgeMap -- ^ The edge map is necessary 
        -- for appointing edge attributes
        , neighbors :: Neighbors -- ^ The `Adjacency List`
        }

edgeExists :: Graph -> Edge -> Bool
edgeExists g e = M.member e (edgeMap g)

-- | Gives the position of the edge to the edges list
edgeIndex :: Graph -> Edge -> Maybe Int
edgeIndex g e = M.lookup e $ edgeMap g

edges :: Graph -> [Edge]
edges g = 
  fmap fst $ M.toList $ edgeMap g

edgeMapFromEdges :: [Edge] -> EdgeMap
edgeMapFromEdges es =
  M.fromList $ zip es [1..]

from :: Edge -> Vertex
from (Edge s t) = s

to :: Edge -> Vertex
to (Edge s t) = t

fromTuple :: (Vertex, Vertex) -> Edge
fromTuple (s,t) = Edge s t

toTuple :: Edge -> (Vertex, Vertex)
toTuple (Edge s t) = (s,t)

reverseEdge :: Edge -> Edge
reverseEdge (Edge s t) = Edge t s

reverseEdges :: Graph -> [Edge]
reverseEdges g = fmap reverseEdge $ edges g

numVertices :: Graph -> Int
numVertices g = length $ vertices g
numEdges :: Graph -> Int
numEdges g = length $ edges g


instance Eq Graph where
  (==) g1 g2 = (sort (vertices g1) == sort (vertices g2))
               && (sort (edges g1) == sort (edges g2))

instance Show Graph where
  show g = "vertices: " ++ show (vertices g) ++ "\n" ++
            "edges: " ++ show (edges g) ++ "\n"

-- | Graph constructor given a neighbors function
createGraph :: [Vertex] -> Neighbors -> Graph
createGraph vs neis =
  let emap = edgeMapFromEdges $ edgesFromNeighbors neis vs
   in Graph { vertices = vs
            , neighbors = neis
            , edgeMap = emap
            }

-- | Graph constructor given a list of edges
graphFromEdges :: [Edge] -> Graph
graphFromEdges es = 
  let vs = Set.toList $ foldl' (\ac (Edge u v) ->
             Set.insert u (Set.insert v ac)) Set.empty es
      esmap = edgeMapFromEdges es
      neimap = IM.fromList 
                  $ fmap 
                    (\v -> 
                      let nes = fmap to 
                                $ M.keys 
                                  $ M.filterWithKey 
                                    (\e _ -> from e == v) 
                                    esmap
                       in (v, nes))
                    vs
      neis = (\v -> 
                 let mns = IM.lookup v neimap
                  in case mns of
                       Nothing -> []
                       Just ns -> ns)
   in Graph { vertices = vs
            , edgeMap = esmap
            , neighbors = neis
            }

edgesFromNeighbors :: Neighbors -> [Vertex] -> [Edge]
edgesFromNeighbors neis vs = 
  let allneis = fmap (\v -> (v,neis v)) vs
   in foldr (\(v,nv) ac -> 
             (fmap (\n -> Edge v n) nv) ++ ac
             ) [] allneis

adjacentEdges :: Graph -> Vertex -> [Edge]
adjacentEdges g v = fmap (\n -> Edge v n) $ neighbors g v

adjacencyMap :: Graph -> IM.IntMap [Vertex]
adjacencyMap g = IM.fromList $ fmap (\v -> (v, (neighbors g v))) vs
                 where vs = vertices g

reverseGraph :: Graph -> Graph
reverseGraph g =
  graphFromEdges $ reverseEdges g

-- | Get the subgraph of a graph by including vertices satisfying given predicate.
filterVertices :: (Vertex -> Bool) -- ^ filter predicate
               -> Graph
               -> Graph
filterVertices f g =
  let oldvs = vertices g
      vs = filter f oldvs 
      neis v = 
        let ns = neighbors g v
         in filter f ns
   in createGraph vs neis

-- | Get the subgraph of a graph by including edges satisfying given predicate.
filterEdges :: (Edge -> Bool) -> Graph -> Graph
filterEdges f g =
  let vs = vertices g
      neis v = 
        let neis = neighbors g v
         in filter (\n -> f (Edge v n)) neis
   in createGraph vs neis

-- | Make a graph undirected by adding all missing reverse edges.
makeUndirected :: Graph -- ^ directed graph
               -> Graph -- ^ undirected graph
makeUndirected g =
  let rg = reverseGraph g
      vs = vertices g
      newnei v = 
        let nei = neighbors g v
            rnei = neighbors rg v
         in sortUniq $ nei ++ rnei
   in createGraph vs newnei

-- | Make a graph directed by removing randomly reverse edges
removeReverseEdges :: Graph -- ^ Graph with reverse edges
                   -> Graph -- ^ Directected graph
removeReverseEdges g =
  let unes = sort $ edges g
      dires = filter (\e -> elem (reverseEdge e) 
                             (filter (\e' -> e' > e) unes) 
                     ) unes
   in graphFromEdges dires


-- | Complete undirected graph from number of vertices
completeGraph :: Int -> Graph
completeGraph n =
  let es = [e | e <- Edge <$> [1..n] <*> [1..n], (\(Edge s t) -> s /= t ) e]
   in graphFromEdges es
