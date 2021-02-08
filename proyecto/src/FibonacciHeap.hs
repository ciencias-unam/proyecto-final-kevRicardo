module FibonacciHeap where

    import BinomialHeap

    {-- | Representación de un Heap Fibonacci
    tam. El tamaño del heap
    mTree. Árbol mínimo del heap
    arboles. Subárboles del hijo
    --}
    data FHeap a = Empty | FHeap {tam :: Int, mTree :: BTree a, arboles :: [BTree a]}
                    deriving (Eq, Show, Ord)

    -- | Instancia Functor a Fibonacci Heap
    instance Functor FHeap where
        fmap f Empty = Empty
        fmap f fheap = FHeap (tam fheap)
                             (f <$> (mTree fheap))
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

    ordena :: (Ord a) => FHeap a -> FHeap a -> FHeap a
    ordena Empty fh = fh
    ordena fh@(FHeap t1 m1 a1) fheap = case fheap of
        Empty -> fh
        (FHeap t2 m2 a2) -> if raiz m1 < raiz m2
            then FHeap (t1+t2) m1 (m2:a1++a2)
            else FHeap (t1+t2) m2 (m1:a1++a2)
 
    inserta :: (Ord a) => FHeap a -> a -> FHeap a
    inserta fib elem = ordena fib $ pure elem

    liga :: (Ord a) => BTree a -> BTree a -> BTree a
    liga t1@(Node ra1 r1 h1) t2@(Node ra2 r2 h2)
        | r1 < r2 = Node (ra1+1) r1 (t2:h1)
        | otherwise = Node (ra1+1) r2 (t1:h2)

    minimo :: (Ord a) => FHeap a -> a
    minimo = raiz . mTree