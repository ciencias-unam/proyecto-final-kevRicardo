module FibonacciHeap where

    {-- | Representación de Heap Binomial
    rango. El rango que tendrá el heap
    raíz. El elemento raíz del heap
    hijos. Subárboles hijos del heap
    --}
    data BTree a = Node { rango :: Int,
                        raiz :: a,
                        hijos :: [BTree a] } 
                        deriving (Eq, Show)

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
                             deriving (Eq, Show)

    -- | Instancia Functor a Fibonacci Heap
    instance Functor FHeap where
        fmap f Empty = Empty
        fmap f (FHeap t m a) = FHeap t (fmap f m) (map (fmap f) a)

    -- | Instancia Applicative a Fibonacci Heap
    instance Applicative FHeap where
        pure a = FHeap 1 (Node 1 a []) []

        Empty <*> _ = Empty
        (FHeap t m a) <*> fhy = m <*> fhy