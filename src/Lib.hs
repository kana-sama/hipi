module Lib
  ( main,
  )
where

import Language.Hipi.AST
import Language.Hipi.Compile
import Language.Hipi.Core.Check
import Language.Hipi.Core.Pretty
import Language.Hipi.Pretty

example :: Module
example =
  Module
    [ DataDeclaration "Bool" [ConstructorDeclaration "True", ConstructorDeclaration "False"],
      Definition
        (Typed "not" (Arr (Var "Bool") (Var "Bool")))
        ( Lam
            [Untyped "x"]
            ( Case
                (Var "x")
                [ (ConstructorPattern "True", Var "False"),
                  (ConstructorPattern "False", Var "True")
                ]
            )
        ),
      Definition Wildcard (App (Var "not") (App (Var "not") (Var "True"))),
      Definition Wildcard U
    ]

main = do
  putStrLn "\nBefore:\n"
  putStrLn $ pretty 0 example
  putStrLn "\nStep 1:\n"
  putStrLn $ pretty 0 $ caseToElim . dataToValue $ example
  putStrLn "\nStep 2:\n"
  putStrLn $ pretty 0 $ defsToLet . caseToElim . dataToValue $ example
  putStrLn "\nStep 3:\n"
  putStrLn $ pretty 0 $ letsToLambdas . defsToLet . caseToElim . dataToValue $ example
  putStrLn "\nStep 4:\n"
  print $ toCore . letsToLambdas . defsToLet . caseToElim . dataToValue $ example
  putStrLn "\nCheck:\n"
  print $ infer [] $ toCore . letsToLambdas . defsToLet . caseToElim . dataToValue $ example
  putStrLn ""
