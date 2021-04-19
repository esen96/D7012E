-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

-- ilaese-8@student.ltu

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR 
      deriving (Eq, Ord, Show)

-- String to expression
parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

-- Expression to string
unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App s e) = "(" ++ s ++ unparse e ++ ")"

{- 
  Evaluates an expression with regards to its associated values. 
  The (String,Float) pair is used for evaluating a variable (String) at
  a specific point (Float) in a graph. Constants are treated numerically
  and the (String,Float) pair is ignored.
  
  Example: eval (App "sin" (Var "x")) [("x", 2.0)] = 0.9092974
  Example: eval (Op "+" (Const 2) (Const 3)) [] = 5.0
-}
eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-- App evaluations
eval (App "sin" x) env = sin (eval x env) 
eval (App "cos" x) env = cos (eval x env) 
eval (App "log" x) env = log (eval x env) 
eval (App "exp" x) env = exp (eval x env) 

-- Differentiates EXPR2 with respect to EXPR1
diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
-- Outer derivative * inner derivative for App expressions
diff v (App "sin" x) = Op "*" (App "cos" x) (diff v x)
diff v (App "cos" x) = Op "*" (Op "-" (Const 0) (App "sin" x)) (diff v x) 
diff v (App "log" x) = Op "*" (Op "/" (Const 1) x) (diff v x)
diff v (App "exp" x) = Op "*" (App "exp" x) (diff v x)
diff _ _ = error "can not compute the derivative"

-- Simplifies an EXPR according to simplification rules
simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
-- parser simplification for function applications
simplify (App fun arg) = 
  let special = simplify arg in
    case (fun, special) of
      ("sin", Const 0) -> Const 0
      ("cos", Const 0) -> Const 1 
      ("exp", Const 0) -> Const 1 
      ("log", Const 1) -> Const 0 
      (f, x)           -> App f x

-- Function generator
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, (Var var)) = \x -> eval body [(var, x)]

-- Function solver
findzero :: String -> String -> Float -> Float
findzero var fun val = helper f f' val
  where
    x   = Var var
    f   = mkfun ((parse fun), x)
    f'  = mkfun ((diff x (parse fun)), x)

helper :: (Float -> Float) -> (Float -> Float) -> Float -> Float
helper f f' x0
  | diff < 0.0001   = x
  | otherwise       = helper f f' x
    where 
      diff = abs (x-x0)
      x    = x0  - ((f x0)/(f' x0))

test1 = findzero "x" "x*x*x+x-1" 1.0
test2 = findzero "y" "cos(y)*sin(y)" 2.0
