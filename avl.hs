data AVLtree a = Node Int a (AVLtree a) (AVLtree a) | Leaf
  deriving (Show, Eq)

node x l r = Node h x l r
  where h = max (height l) (height r) + 1

height Leaf = 0
height (Node h _ _ _) = h

balance Leaf = 0
balance (Node _ _ l r) = height l - height r

contains :: (Ord a) => a -> AVLtree a -> Bool
contains x Leaf = False
contains x (Node _ y l r)
  | x == y  = True
  | x < y   = contains x l
  | x > y   = contains x r

max (Node _ x _ Leaf) = x
max (Node _ _ _ r) = Main.max r

min (Node _ x Leaf _) = x
min (Node _ _ l _) = Main.min l

insert :: (Ord a) => a -> AVLtree a -> AVLtree a
insert x tree = foldr balanceIns (node x Leaf Leaf) $ trace x tree

trace :: (Ord a) => a -> AVLtree a -> [AVLtree a]
trace x Leaf = []
trace x n@(Node _ y l r)
  | x == y  = [n]
  | x < y   = n : trace x l
  | x > y   = n : trace x r

balanceIns :: (Ord a) => AVLtree a -> AVLtree a -> AVLtree a
balanceIns p@(Node b x l r) n@(Node _ y _ _)
  | y < x =
    let p' = node x n r in
      if abs (balance p') < 2
      then p'
      else
        if balance n == -1
        then balanceIns p' $ rotateLeft n
        else rotateRight p'
  | y > x =
    let p' = node x l n in
      if abs (balance p') < 2
      then p'
      else if balance n == 1
        then balanceIns p' $ rotateRight n
        else rotateLeft p'
  | otherwise = p

remove :: (Ord a) => a -> AVLtree a -> AVLtree a
remove x tree = foldr1 balanceDel $ delTrace x tree

delTrace :: (Ord a) => a -> AVLtree a -> [AVLtree a]
delTrace x Leaf = []
delTrace x n@(Node _ y l r)
  | x < y = let xs@(l':_) = delTrace x l in node y l' r : xs
  | x > y = let xs@(r':_) = delTrace x r in node y l r' : xs
  | x == y =
    if l == Leaf
    then [r]
    else let l' = remove iop l in [node iop l' r, l']
      where iop = Main.max l

balanceDel :: (Ord a) => AVLtree a -> AVLtree a -> AVLtree a
balanceDel p@(Node b x l r) n
  | n `rightChildOf` p  =
    let p' = node x l n in
      if abs (balance p') < 2
      then p'
      else
        if balance l == -1
        then balanceDel (node x (rotateLeft l) r) n
        else rotateRight p'
  | n `leftChildOf` p =
    let p' = node x n r in
      if abs (balance p') < 2
      then p'
      else
        if balance r == 1
        then balanceDel (node x l (rotateRight r)) n
        else rotateLeft p'

rightChildOf Leaf (Node _ _ _ Leaf) = True
rightChildOf (Node _ x _ _) (Node _ y _ _) = x > y
rightChildOf _ _ = False

leftChildOf Leaf (Node _ _ Leaf _) = True
leftChildOf (Node _ x _ _) (Node _ y _ _) = x < y
leftChildOf _ _ = False

rotateLeft (Node _ x a (Node _ y b c)) = node y (node x a b) c

rotateRight (Node _ x (Node _ y a b) c) = node y a (node x b c)
