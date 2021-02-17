module Dijkstra where

    import Graph
    import FibonacciHeap

    -- | Algoritmo Dijkstra
    dijkstra :: (Ord a) => Graph a -> [a]
    dijkstra graph = case graph of
        Empty -> []
        (Graph (((e1,e1),cost):gs)) ->