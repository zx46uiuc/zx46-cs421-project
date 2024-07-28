data Tree = Leaf Int | Node Tree Tree deriving (Show)

sumTree :: Tree -> Int
sumTree (Leaf n) = n
sumTree (Node left right) = sumTree left + sumTree right

doubleTree :: Tree -> Tree
doubleTree (Leaf n) = Leaf (2 * n)
doubleTree (Node left right) = Node (doubleTree left) (doubleTree right)

main :: IO ()
main = do
  let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
  print (sumTree tree)
  print (doubleTree tree)
