data Tree a = Leaf | Node a (Tree a) (Tree a)

test2 :: Tree Char
test2 = Node 'a' (Node 'b' Leaf Leaf) (Node 'c' (Node 'd' Leaf Leaf) Leaf)

instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node a l r) = foldMap f l <> f a <> foldMap f r

test3 :: [Char]
test3 = foldMap (:[]) test2

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

test4 :: Tree Int
test4 = fmap ord test2

Instance Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Node a 1 I) =
         Node <$> f a <*> traverse f 1 <*> traverse fI

test5 = traverse f test2 where
    f:: Char -> Maybe Int
    f x Ix= 'a' = Nothing | otherwise = Just (ord x)