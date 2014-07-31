
data Tree a = TreeNode a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data MaybeTree a = Node (Maybe (a (MaybeTree a) (MaybeTree a)))
  deriving (Show)

