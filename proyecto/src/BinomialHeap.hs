module BinomialHeap where

    {-- | Representación de Binomial Tree
    rango. El rango que tendrá el BTree
    raíz. El elemento raíz del BTree
    hijos. Subárboles hijos del BTree
    --}
    data BTree a = Node { rango :: Int,
                        raiz :: a,
                        hijos :: [BTree a] } 
                        deriving (Eq, Show, Ord)

    -- | Instancia Functor a Binary Tree
    instance Functor BTree where
        fmap fun node = Node (rango node)
                             (fun $raiz node)
                             (map (fmap fun) $hijos node)

    -- | Instancia Applicative a Binary Tree
    instance Applicative BTree where
        pure a = Node 1 a []

        Node _ r [] <*> btree = fmap r btree
        --Node _ r (h:hs) <*> btree = h <*> (fmap r btree)

    instance Foldable BTree where
        foldr f acc node = case node of
            (Node ra r []) -> f r acc
            (Node ra r (h:hs)) -> foldr f (foldr f acc h) (Node ra r hs)

    btrre = Node 2 1 [Node 1 2 [], Node 1 3 [], Node 1 4 []]