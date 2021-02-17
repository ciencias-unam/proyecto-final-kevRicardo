module Graph where

    type Cost = Int

    data Graph a = Empty | Graph { 
        edges :: [((a,a), Cost)] }
        deriving (Eq, Show, Ord)

    -- | Obtiene una gráfica a partir de una lista
    fromList :: (Ord a) => [] -> Graph a
    fromList = foldr 