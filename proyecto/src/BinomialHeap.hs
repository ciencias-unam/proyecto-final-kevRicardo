-- | Binommial Heap
module BinomialHeap where

    import Data.List

    {-- | Representación de Binomial Tree
        rango. El rango que tendrá el BTree
        raíz. El elemento raíz del BTree
        hijos. Subárboles hijos del BTree
    --}
    data BTree a = Node {
        rango :: Int,
        raiz :: a,
        marca :: Bool,
        hijos :: [BTree a] }
        deriving (Eq, Show, Ord)

    instance Show BTree where
        show (Node r ra m h) = ""

    -- | Instancia Functor a Binomial Tree
    instance Functor BTree where
        fmap fun node = Node (rango node)
                             (fun $ raiz node)
                             (marca node)
                             (map (fmap fun) $ hijos node)

    -- | Instancia Applicative a Binomial Tree
    instance Applicative BTree where
        pure a = Node 1 a False []

        Node _ r m [] <*> btree = fmap r btree
        --Node ra r l@(h:hs) <*> btree = (Node ra r hs) <*> h <*> btree

    -- | Instancia Foldable a Binomial Tree
    instance Foldable BTree where
        foldr f acc node = case node of
            (Node ra r m []) -> f r acc
            (Node ra r m (h:hs)) -> foldr f (foldr f acc h) (Node ra r m hs)

    -- | Liga dos Binomial Tree
    ligaBTree :: (Ord a) => BTree a -> BTree a -> BTree a
    ligaBTree n1@(Node r1 ra1 m1 h1) n2@(Node _ ra2 m2 h2)
        | ra1 < ra2 = Node (r1+1) ra1 m1 (n2:h1)
        | otherwise = Node (r1+1) ra2 m2 (n1:h2)

    -- | Fusiona una lista de Binomial Tree con el mismo rango
    fusionaBTree :: (Ord a) => [BTree a] -> [BTree a]
    fusionaBTree ts = foldl fusiona [] ts
      where
        -- ^ Fusiona los árboles con el mismo rango
        fusiona [] btree = [btree]
        fusiona btree@(x:xs) t = if rango x == rango t 
            then fusionaBTree $ (ligaBTree t x):xs
            else t:btree

    -- | Obtiene la tupla con el Binomial Tree mínimo y el resto del
    -- Binomial Tree
    getMinimo :: (Ord a) => [BTree a] -> (BTree a, [BTree a])
    getMinimo [t] = (t, [])
    getMinimo (x:xs) = if raiz x < raiz btree
        then (x,xs)
        else (btree, x:btrees)
      where
        -- ^ Obtiene el árbol mínimo y el resto de árboles
        (btree, btrees) = getMinimo xs

    mostrar :: (Show a) => BTree a -> String
    mostrar (Node r ra m h) = case h of
        [] -> if m then show ra ++ "*" else show ra
        otherwise -> if m 
            then show ra ++ "*" ++ "[" ++ (foldr ((++) . mostrar) "" h) ++ "]"
            else show ra ++ "[" ++ (foldr ((++) . mostrar) "" h) ++ "]"

    nodes = [Node 1 2 True [], Node 1 3 False [], Node 1 4 False []]

    corteBTree :: (Ord a) => [BTree a] -> BTree a -> (BTree a, [BTree a])
    corteBTree btrees btree = if elem btree btrees
        then (btree, btrees \\ [btree])
        else (btree, [])