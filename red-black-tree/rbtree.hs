module RBTree where


data Color = Red | Black deriving Show
data Tree a = Empty | Node Color a (Tree a) (Tree a) deriving Show

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node _ v l r) = flatten l ++ v : flatten r

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x (Node _ a l r)
        | x==a = True
        | x<a = member x l
        | otherwise = member x r


blackenRoot :: Tree a -> Tree a
blackenRoot Empty = Empty
blackenRoot (Node _ a l r) = Node Black a l r


redenRoot :: Tree a -> Tree a
redenRoot Empty = Empty
redenRoot (Node _ a l r) = Node Red a l r

insertBst :: (Ord a) => a -> Tree a -> Tree a
insertBst x Empty = Node Red x Empty Empty
insertBst x t@(Node color v l r)
        | x==v = t
        | x<v = balance color v (insertBst x l) r
        | otherwise = balance color v l (insertBst x r)         


insert :: (Ord a) => a -> Tree a -> Tree a
insert a t = blackenRoot (insertBst a t)


balance :: Color -> a -> Tree a -> Tree a -> Tree a
balance Black _ (Node Red y (Node Red x a b) c) d
    = Node Red y (Node Black x a b) (Node Black y c d)
balance Black _ (Node Red x a (Node Red y b c)) d
    = Node Red y (Node Black x a b) (Node Black y c d)
balance Black x a (Node Red _ (Node Red y b c) d)
    = Node Red y (Node Black x a b) (Node Black y c d)
balance Black x a (Node Red y b (Node Red _ c d))
    = Node Red y (Node Black x a b) (Node Black y c d)
balance color v l r 
    = Node color v l r


balanceT :: Tree a -> Tree a
balanceT Empty = Empty
balanceT (Node color v l r) = balance color v l r


delete :: (Ord a) => a -> Tree a -> Tree a
delete x t = blackenRoot (deleteBst x t)


deleteBst :: (Ord a) => a -> Tree a -> Tree a
deleteBst _ Empty = Empty
deleteBst x t@(Node _ v l r)
    | x<v = delL x t
    | x>v = delR x t
    | otherwise = fuse l r


balL :: Tree a -> Tree a
balL (Node Black y (Node Red x a b) c)
    = Node Red y (Node Black x a b) c
balL (Node Black y a (Node Black z b c))
    = balanceT (Node Black y a (Node Red z b c))
balL (Node Black x a (Node Red z (Node Black y b c) d@(Node Black _ _ _)))
    = Node Red y (Node Black x a b) (balanceT (Node Black z c (redenRoot d)))
balL x = x

balR :: Tree a -> Tree a
balR (Node Black y a (Node Red x b c))
    = Node Red y a (Node Black x b c)
balR (Node Black y (Node Black z a b) c)
    = balanceT (Node Black y (Node Red z a b) c)
balR (Node Black x (Node Red z a@(Node Black _ _ _) (Node Black y b c)) d)
    = Node Red y (balanceT (Node Black x (redenRoot a) b)) (Node Black z c d)
balR x = x


delL :: (Ord a) => a -> Tree a -> Tree a
delL x (Node Red y l r) = Node Red y (deleteBst x l) r
delL x (Node Black y l r) = balL $ Node Black y (deleteBst x l) r

delR :: (Ord a) => a -> Tree a -> Tree a
delR x (Node Red y l r) = Node Red y l (deleteBst x r)
delR x (Node Black y l r) = balR $ Node Black y l (deleteBst x r)

fuse :: (Ord a) => Tree a -> Tree a -> Tree a
fuse Empty t = t
fuse t Empty = t
fuse t@(Node Black _ _ _) (Node Red y l2 r2) = Node Red y (fuse t l2) r2
fuse (Node Red x l1 r1) t@(Node Black _ _ _ ) = Node Red x l1 (fuse r1 t)
fuse (Node Red x l1 r1) (Node Red y l2 r2) =
  let s = fuse r1 l2
   in case s of
        (Node Red z s1 s2) -> Node Red z (Node Red x l1 s1) (Node Red y s2 r2)
        (Node Black _ _ _) -> Node Red x l1 (Node Red y s r2)
fuse (Node Black x l1 r1) (Node Black y l2 r2) =
  let s = fuse r1 l2
   in case s of
        (Node Red z s1 s2) -> Node Red z (Node Black x l1 s1) (Node Black y s2 r2)
        (Node Black _ _ _) -> balL (Node Black x l1 (Node Black y s r2))
