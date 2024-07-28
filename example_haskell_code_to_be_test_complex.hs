data Tree = Leaf Int | Node Tree Tree deriving (Show)

sumTree :: Tree -> Int
sumTree (Leaf n) = n
sumTree (Node left right) = sumTree left + sumTree right

doubleTree :: Tree -> Tree
doubleTree (Leaf n) = Leaf (2 * n)
doubleTree (Node left right) = Node (doubleTree left) (doubleTree right)

applyAndPrintTree :: (Tree -> Tree) -> Tree -> IO ()
applyAndPrintTree f tree = do
  let caseResult = f tree
  let newTree = Node caseResult (Leaf 0)
  printTree newTree

printTree :: Tree -> IO ()
printTree (Leaf n) = print n
printTree (Node left right) = do
  printTree left
  printTree right

main :: IO ()
main = do
  let tree = Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 3) (Leaf 4)))
  print (sumTree tree)
  print (doubleTree tree)
  applyAndPrintTree doubleTree tree