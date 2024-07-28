-- AST representation (simplified for demonstration purposes)
data Expr
  = Var String -- A variable represented by a string
  | Constr String [Expr] -- A constructor with a name and a list of expressions
  | App Expr Expr -- Application of one expression to another
  | Lam [String] Expr -- Lambda abstraction with a list of bound variables and an expression
  | Let [(String, Expr)] Expr -- Let bindings with a list of variable-expression pairs and a body expression
  deriving (Show, Eq)

-- Example AST for a simple demo Haskell program
demoASTSimple :: [Expr]
demoASTSimple =
  [ Constr
      "Node"
      [ Constr "Leaf" [Var "1"]
      , Constr "Node" [Constr "Leaf" [Var "2"], Constr "Leaf" [Var "3"]]
      ]
  , Lam ["tree"] (App (Var "sumTree") (Var "tree"))
  , Lam ["tree"] (App (Var "doubleTree") (Var "tree"))
  ]

-- Example AST for a more complex demo Haskell program
demoASTComplex :: [Expr]
demoASTComplex =
  [ Constr
      "Node"
      [ Constr "Leaf" [Var "1"]
      , Constr
          "Node"
          [ Constr "Leaf" [Var "2"]
          , Constr "Node" [Constr "Leaf" [Var "3"], Constr "Leaf" [Var "4"]]
          ]
      ]
  , Lam ["tree"] (App (Var "sumTree") (Var "tree"))
  , Lam ["tree"] (App (Var "doubleTree") (Var "tree"))
  , Lam
      ["f", "tree"]
      (Let
         [ ("caseResult", App (Var "f") (Var "tree"))
         , ( "newTree"
           , Constr "Node" [Var "caseResult", Constr "Leaf" [Var "0"]])
         ]
         (App (Var "printTree") (Var "newTree")))
  ]

-- Current demo AST used for testing
demoAST :: [Expr]
demoAST = demoASTComplex

-- Metric 1: Pattern Size (PSIZ)
patternSize :: Expr -> Int
patternSize (Var _) = 1
patternSize (Constr _ exprs) = 1 + sum (map patternSize exprs)
patternSize (App e1 e2) = patternSize e1 + patternSize e2
patternSize (Lam _ e) = patternSize e
patternSize (Let bindings e) =
  sum (map (patternSize . snd) bindings) + patternSize e

-- Metric 2: Number of Pattern Variables (NPVS)
numPatternVars :: Expr -> Int
numPatternVars (Var _) = 1 -- variable is contributing to variable.
numPatternVars (Constr _ exprs) = sum (map numPatternVars exprs)
numPatternVars (App e1 e2) = numPatternVars e1 + numPatternVars e2
numPatternVars (Lam vars e) = length vars + numPatternVars e
numPatternVars (Let bindings e) =
  sum (map (numPatternVars . snd) bindings) + numPatternVars e

-- Metric 3: Number of Constructors (PATC)
numConstructors :: Expr -> Int
numConstructors (Var _) = 0
numConstructors (Constr _ exprs) = 1 + sum (map numConstructors exprs) -- constructor contribute to 1
numConstructors (App e1 e2) = numConstructors e1 + numConstructors e2
numConstructors (Lam _ e) = numConstructors e
numConstructors (Let bindings e) =
  sum (map (numConstructors . snd) bindings) + numConstructors e

-- Metric 4: Depth of Nesting (DON)
depthOfNesting :: Expr -> Int
depthOfNesting (Var _) = 1
depthOfNesting (Constr _ exprs) = 1 + maximum (map depthOfNesting exprs)
depthOfNesting (App e1 e2) = 1 + max (depthOfNesting e1) (depthOfNesting e2)
depthOfNesting (Lam _ e) = 1 + depthOfNesting e
depthOfNesting (Let bindings e) =
  1 + max (maximum (map (depthOfNesting . snd) bindings)) (depthOfNesting e)

--- Function Call
-- Metric 4: Pathcount (PATH)
pathCount :: Expr -> Int
pathCount (Var _)          = 1
pathCount (Constr _ exprs) = 1 + sum (map pathCount exprs)
pathCount (App e1 e2)      = pathCount e1 * pathCount e2 -- all possible paths between e1 and e2.
pathCount (Lam _ e)        = pathCount e
pathCount (Let bindings e) = sum (map (pathCount . snd) bindings) + pathCount e

-- Distance Metrics
-- Distance Metric 1: Number of New Scopes
numNewScopes :: Expr -> Int
numNewScopes (Var _) = 0
numNewScopes (Constr _ exprs) = sum (map numNewScopes exprs)
numNewScopes (App e1 e2) = numNewScopes e1 + numNewScopes e2
numNewScopes (Lam _ e) = 1 + numNewScopes e
numNewScopes (Let bindings e) =
  1 + sum (map (numNewScopes . snd) bindings) + numNewScopes e

-- Distance Metric 2: Number of Declarations Brought into Scope
numDeclarationsInScope :: Expr -> Int
numDeclarationsInScope (Var _) = 0
numDeclarationsInScope (Constr _ exprs) = sum (map numDeclarationsInScope exprs)
numDeclarationsInScope (App e1 e2) =
  numDeclarationsInScope e1 + numDeclarationsInScope e2
numDeclarationsInScope (Lam vars e) = length vars + numDeclarationsInScope e
numDeclarationsInScope (Let bindings e) =
  length bindings
    + sum (map (numDeclarationsInScope . snd) bindings)
    + numDeclarationsInScope e

-- Distance Metric 3: Number of Source Lines
numSourceLines :: Expr -> Int
numSourceLines (Var _) = 1
numSourceLines (Constr _ exprs) = 1 + sum (map numSourceLines exprs)
numSourceLines (App e1 e2) = 1 + numSourceLines e1 + numSourceLines e2
numSourceLines (Lam _ e) = 1 + numSourceLines e
numSourceLines (Let bindings e) =
  1
    + length bindings
    + sum (map (numSourceLines . snd) bindings)
    + numSourceLines e

-- Distance Metric 4: Number of Parse Tree Nodes
numParseTreeNodes :: Expr -> Int
numParseTreeNodes (Var _) = 1
numParseTreeNodes (Constr _ exprs) = 1 + sum (map numParseTreeNodes exprs)
numParseTreeNodes (App e1 e2) = 1 + numParseTreeNodes e1 + numParseTreeNodes e2
numParseTreeNodes (Lam _ e) = 1 + numParseTreeNodes e
numParseTreeNodes (Let bindings e) =
  1
    + length bindings
    + sum (map (numParseTreeNodes . snd) bindings)
    + numParseTreeNodes e

main :: IO ()
main = do
  putStrLn
    $ "Pattern size (PSIZ) of the demo AST: "
        ++ show (sum (map patternSize demoAST))
  putStrLn
    $ "Number of pattern variables (NPVS) of the demo AST: "
        ++ show (sum (map numPatternVars demoAST))
  putStrLn
    $ "Number of constructors (PATC) of the demo AST: "
        ++ show (sum (map numConstructors demoAST))
  putStrLn
    $ "Pathcount (PATH) of the demo AST: " ++ show (sum (map pathCount demoAST))
  putStrLn
    $ "Depth of nesting (DON) of the demo AST: "
        ++ show (sum (map depthOfNesting demoAST))
  putStrLn
    $ "Number of new scopes in demoAST: "
        ++ show (sum (map numNewScopes demoAST))
  putStrLn
    $ "Number of declarations brought into scope in demoAST: "
        ++ show (sum (map numDeclarationsInScope demoAST))
  putStrLn
    $ "Number of source lines in demoAST: "
        ++ show (sum (map numSourceLines demoAST))
  putStrLn
    $ "Number of parse tree nodes in demoAST: "
        ++ show (sum (map numParseTreeNodes demoAST))
