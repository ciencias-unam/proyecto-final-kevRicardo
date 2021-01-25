module FibonacciHeap where

    {-- | Representación de Heap Binomial
    rango. El rango que tendrá el heap
    raíz. El elemento raíz del heap
    hijos. Subárboles hijos del heap
    --}
    data BTree a = Node { rango :: Int,
                        raiz :: a,
                        hijos :: [BTree a] } 
                        deriving (Eq, Show, Ord)

    -- | Instancia Functor a Binary Tree
    instance Functor BTree where
        fmap f (Node ra r h) = Node ra (f r) (map (fmap f) h)

    -- | Instancia Applicative a Binary Tree
    instance Applicative BTree where
        pure a = Node 1 a []

        --Node _ r [] <*> btree = fmap r btree
        Node _ r h <*> btree = fmap r btree

    {-- | Representación de un Heap Fibonacci
    tam. El tamaño del heap
    mTree. Árbol mínimo del heap
    arboles. Subárboles del hijo
    --}
    data FHeap a = Empty | FHeap { tam :: Int,
                             mTree :: BTree a,
                             arboles :: [BTree a] }
                             deriving (Eq, Show, Ord)

    -- | Instancia Functor a Fibonacci Heap
    instance Functor FHeap where
        fmap f Empty = Empty
        fmap f (FHeap t m a) = FHeap t (fmap f m) (map (fmap f) a)

    -- | Instancia Applicative a Fibonacci Heap
    instance Applicative FHeap where
        pure a = FHeap 1 (Node 1 a []) []

        Empty <*> _ = Empty
        --(FHeap t m a) <*> fhy = fmap m a

    ordena :: (Ord a) => FHeap a -> FHeap a -> FHeap a
    ordena Empty fh = fh
    ordena fh Empty = fh
    ordena (FHeap t1 m1 a1) (FHeap t2 m2 a2)
        | raiz m1 < raiz m2 = FHeap (t1+t2) m1 (m2:a1++a2)
        | otherwise = FHeap (t1+t2) m2 (m1: a1++a2)

    inserta :: (Ord a) => FHeap a -> a -> FHeap a
    inserta fib elem = ordena fib $ pure elem

    liga :: (Ord a) => BTree a -> BTree a -> BTree a
    liga t1@(Node ra1 r1 h1) t2@(Node ra2 r2 h2)
        | r1 < r2 = Node (ra1+1) r1 (t2:h1)
        | otherwise = Node (ra1+1) r2 (t1:h2)

    minimo :: (Ord a) => FHeap a -> a
    minimo = raiz . mTree