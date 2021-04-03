import Prelude hiding (product, and, or, reverse, unzip) 

-- 5.2
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z)
    | x < min y z = (x, min y z, max y z)
    | y < min x z = (y, min x z, max x z)
    | z < min x y = (z, min x y, max x y)

-- 5.10
divisors :: Int -> [Int]
divisors n = [ i | i<-[1..n], mod n i == 0 ]

isPrime :: Int -> Bool
isPrime n
    | divisors n == [1, n]      = True
    | otherwise                 = False

-- 5.11
matches :: Int -> [Int] -> [Int]
matches i j = [ k | k<-j, i == k ]

elem' :: Int -> [Int] -> Bool
elem' i j
    | matches i j /= []     = True 
    | otherwise             = False 

-- 5.18
shift :: ((a, b), c) -> (a, (b, c)) 
shift ((a, b), c) = (a, (b, c))

-- 5.22
onSeparateLines :: [String] -> String
onSeparateLines strings = concat [ chars ++ ['\n'] | chars<-strings ]

-- 5.23
duplicate :: String -> Int -> String 
duplicate str n 
    | n <= 0    = ""
    | otherwise = str ++ duplicate str (n-1)

-- 5.24
pushRight :: Int -> String -> String 
pushRight len str 
    | len - length str <= 0    = str
    | otherwise = " " ++ pushRight (len-1) str  

-- 6.29, Only focusing on calculating discounts
discount :: [Int] -> Int
discount barcodes = div count 2
    where count = length [ sherry | sherry<-barcodes, sherry == 1234 ]

-- 7.2
addFtwo :: [Int] -> Int
addFtwo [] = 0
addFtwo (x:[]) = x
addFtwo (x:xs) = x + head xs

-- 7.3
addFtwo' :: [Int] -> Int 
addFtwo' xs
    | length xs <= 0      = 0
    | length xs == 1      = head xs
    | otherwise           = head xs + head (tail xs)

-- 7.4
product :: [Int] -> Int
product []      = 1
product (x:xs)  = x * product xs

-- foldr alternative
product' :: [Int] -> Int
product' lst = foldr (*)1 lst

-- 7.5
and, or :: [Bool] -> Bool 
and []     = True 
and (x:xs) = x && and xs

or []      = False 
or (x:xs)  = x || or xs

-- 7.7
unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
    | xs == cleared    = x : unique cleared
    | otherwise        = unique cleared
    where 
        cleared = clearDupes x xs 

clearDupes :: Int -> [Int] -> [Int]
clearDupes i lst = [ j | j<-lst, j /= i ]

-- 7.8
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]  

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip (x:xs) = (fst x : fst (unzip xs), snd x : snd (unzip xs))

-- 7.9
-- use head,last on the sorted list
-- could use min, max

-- 7.14
drp :: Int -> [a] -> [a]
drp 0 lst     = lst
drp _ []      = []
drp n (x:xs) 
    | n>0       = drp (n-1) xs

splAt :: Int -> [a] -> ([a],[a])
splAt 0 lst    = ([],lst)
splAt _ []     = ([],[])
splAt n (x:xs) = ( x : fst splt, snd splt ) 
    where 
        splt = (splAt (n-1) xs) 

-- 7.18
sublist :: String -> String -> Bool 
sublist "" _       = True
sublist _ ""       = False
sublist (x:xs) ys  = elem x ys && sublist xs ys 
