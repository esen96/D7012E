data Temp = Cold | Hot deriving Show
data Season = Spring | Summer | Autumn | Winter

-- 14.1
weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold


-- 14.4 
perimeter :: Shape -> Float
perimeter (Circle r)        = 2*pi*r
perimeter (Rectangle h w)   = 2*h*w
perimeter (Triangle a b c)  = a+b+c


-- 14.5 
data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving (Ord,Show,Read)

isRound :: Shape -> Bool 
isRound (Circle _ )      = True 
isRound (Rectangle _ _)  = False
isRound (Triangle _ _ _) = False  


area :: Shape -> Float 
area (Circle r)         = pi*r*r
area (Rectangle h w )   = h*w 
area (Triangle a b c)   = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c) / 2 


-- 14.6
isRegular :: Shape -> Bool 
isRegular (Circle _)        = True
isRegular (Rectangle h w)   = h == w
isRegular (Triangle a b c)  = a == b && b == c && c == a  

-- 14.8
instance Eq Shape where 
    (Circle a) == (Circle b) = (a == b) || (a < 0 || b < 0)
    (Rectangle h1 w1) == (Rectangle h2 w2) = ( (h1 == h2) && (w1 == w2)) || (h1 < 0)
                                             || (h2 < 0) || (w1 < 0) || (w2 < 0)
    (Triangle a b c) == (Triangle d e f) = ((a == d) && (b == e) && (c == f)) 
                                            || (a<0) || (b<0) || (c<0) || (d<0) || (e<0)
                                            || (f<0)


-- 14.16
data Expr = Lit Int | 
            Add Expr Expr | 
            Sub Expr Expr |
            Mul Expr Expr |
            Div Expr Expr 

-- 14.17
size :: Expr -> Int
size (Lit n)          = 0
size (Add e1 e2)      = 1 + size e1 + size e2
size (Sub e1 e2)      = 1 + size e1 + size e2
size (Mul e1 e2)      = 1 + size e1 + size e2
size (Div e1 e2)      = 1 + size e1 + size e2

eval :: Expr -> Int 
eval (Lit n)            = n 
eval (Add e1 e2)        = (eval e1) + (eval e2)
eval (Sub e1 e2)        = (eval e1) - (eval e2)
eval (Mul e1 e2)        = (eval e1) * (eval e2)
eval (Div e1 e2)
    | eval e2 == 0      = error "Divide by zero"
    | otherwise         = div (eval e1) (eval e2)

instance Show Expr where
    show (Lit n) = show n
    show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
    show (Div e1 e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"

-- 14.18
data Expr2 = Lit2 Int |
             Op Ops Expr2 Expr2

data Ops = Add2 | Sub2 | Mul2 | Div2

eval2 :: Expr2 -> Int 
eval2 (Lit2 n)              = n 
eval2 (Op Add2 e1 e2)       = (eval2 e1) + (eval2 e2)
eval2 (Op Sub2 e1 e2)       = (eval2 e1) - (eval2 e2)
eval2 (Op Mul2 e1 e2)       = (eval2 e1) * (eval2 e2)
eval2 (Op Div2 e1 e2)       = div (eval2 e1) (eval2 e2)

-- 14.21
data NTree = NilT |
             Node Int NTree NTree

left :: NTree -> NTree
left (NilT) = NilT
left (Node _ l _) = l 

right :: NTree -> NTree
right (NilT) = NilT
right (Node _ _ r) = r 

-- 14.22
inTree :: Int -> NTree -> Bool
inTree _ NilT           = False 
inTree e (Node et l r)  = (e == et) || inTree e l || inTree e r

-- 14.23
minT, maxT :: NTree -> Int
minT NilT            = error "No value"
minT (Node e l r)    = minHelper (Node e l r) e
    where 
        minHelper :: NTree -> Int -> Int
        minHelper NilT e            = e 
        minHelper (Node et l r) e   = min (min comp minLeft) (min comp minRight)
            where
                comp        = min e et
                minLeft     = minHelper l comp
                minRight    = minHelper r comp



maxT NilT            = error "No value"
maxT (Node e l r)    = maxHelper (Node e l r) e
    where
        maxHelper :: NTree -> Int -> Int
        maxHelper NilT e            = e 
        maxHelper (Node et l r) e   = max (max comp maxLeft) (max comp maxRight)
            where
                comp        = max e et
                maxLeft     = maxHelper l comp
                maxRight    = maxHelper r comp


-- 14.24
reflect :: NTree -> NTree
reflect NilT            = NilT
reflect (Node e l r)    = Node e rf lf
    where 
        rf = reflect r
        lf = reflect l