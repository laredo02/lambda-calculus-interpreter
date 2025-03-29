
module UntypedLambdaCalc where

import qualified Data.Set as Set

data Term = Var String
          | Abs String Term
          | App Term Term

showTerm :: Term -> String
showTerm (Var s) = s
showTerm (Abs s t) = "(λ" ++ s ++ "." ++ showTerm t ++ ")"
showTerm (App t1 t2) = (showTerm t1) ++ (showTerm t2)

putTerm :: Term -> IO()
putTerm t = putStrLn (showTerm t)

allVars :: Term -> Set.Set String
allVars (Var s) = Set.singleton s
allVars (Abs s t) = allVars t
allVars (App t1 t2) = Set.union (allVars t1) (allVars t2)

freeVars :: Term -> Set.Set String
freeVars (Var s) = Set.singleton s
freeVars (Abs s t) = Set.delete s (freeVars t)
freeVars (App t1 t2) = Set.union (freeVars t1) (freeVars t2)


term :: Term
term = ((App (Abs "x" (App (Var "x") (Var "x"))) (Var "y")))




  



