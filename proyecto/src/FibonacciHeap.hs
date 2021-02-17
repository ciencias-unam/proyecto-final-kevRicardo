-- | Fibonacci Heap
module FibonacciHeap where

    import BinomialHeap

    {-- | Representación de un Heap Fibonacci
    tam. El tamaño del heap
    mTree. Árbol mínimo del heap
    arboles. Subárboles del hijo
    --}
    data FHeap a = Empty | FHeap {
        tam :: Int,
        minTree :: BTree a,
        arboles :: [BTree a] }
        deriving (Eq, Show, Ord)

    -- | Instancia Functor a Fibonacci Heap
    instance Functor FHeap where
        fmap f Empty = Empty
        fmap f fheap = FHeap (tam fheap)
                             (f <$> (minTree fheap))
                             (map (f <$>) (arboles fheap))

    -- | Instancia Applicative a Fibonacci Heap
    instance Applicative FHeap where
        pure a = FHeap 1 (pure a) []

        Empty <*> _ = Empty
        --(FHeap t m a) <*> fhy = fmap m a

    -- | Instancia Foldable a Fibonacci Heap
    instance Foldable FHeap where
        foldr f acc Empty = acc
        foldr f acc (FHeap t m []) = foldr f acc m
        foldr f acc (FHeap t m (a:as)) = foldr f (foldr f (foldr f acc m) Empty) (FHeap t a as)

    -- | Une dos F-Heaps en uno nuevo
    funde :: (Ord a) => FHeap a -> FHeap a -> FHeap a
    funde fheap1 fheap2 = case fheap1 of
        Empty -> fheap2
        (FHeap t1 m1 a1) -> case fheap2 of
            Empty -> fheap1
            (FHeap t2 m2 a2) -> if raiz m1 < raiz m2
                then FHeap (t1+t2) m1 (m2:a1++a2)
                else FHeap (t1+t2) m2 (m1:a1++a2)
 
    -- | Agrega un nuevo elemento al F-Heap
    inserta :: (Ord a) => FHeap a -> a -> FHeap a
    inserta fheap e = funde fheap $ pure e

    -- | Obtiene el elemento mínimo
    minimoElemento :: (Ord a) => FHeap a -> a
    minimoElemento = raiz . minTree

    -- | Obtiene el nodo con el elemento mínimo
    getMinElemento :: (Ord a) => FHeap a -> BTree a
    getMinElemento = minTree

    -- | Elimina el elemento mínimo de un Fibonacci Heap
    eliminaMinimo :: (Ord a) => FHeap a -> FHeap a
    eliminaMinimo (FHeap _ (Node _ n m []) []) = Empty
    eliminaMinimo (FHeap t m a) = FHeap (t-1) min arboles
      where
        -- ^ Reacomoda los árboles para mantener al estructura
        (min, arboles) = getMinimo $ fusionaBTree $ (hijos m) ++ a

    decrementaLlave :: (Ord a) => FHeap a -> BTree a -> a -> FHeap a
    decrementaLlave a b c = a

    corte :: (Ord a) => FHeap a -> BTree a -> FHeap a
    corte fh@(FHeap t m a) btree 
        | m == btree = eliminaMinimo fh
        | otherwise = if bts == []
            then (FHeap (t+1) m bts)
            else (FHeap t m bts)
      where
        (bt, bts) = corteBTree a btree

    fibonacci = FHeap 11 (Node 1 0 False []) [
        Node 1 10 False [],
        Node 1 9 False [],
        Node 1 8 False [],
        Node 1 7 False [],
        Node 1 6 False [],
        Node 3 1 False [
            Node 2 3 False [
                Node 1 4 False []],
            Node 1 5 False []],
        Node 1 2 False []]