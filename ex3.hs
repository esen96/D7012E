-- 9.2
len :: [a] -> Int 
len lst = sum x
    where x = map justGiveMeAOne lst

justGiveMeAOne :: a -> Int
justGiveMeAOne _ = 1

-- 9.4
-- addTwo

-- 9.6
sqrs :: [Int] -> [Int]
sqrs lst = map (^2) lst

sumSqrs :: [Int] -> Int
sumSqrs s = sum (sqrs s)

greater :: [Int] -> Bool
greater g = len ls == len g
    where ls = filter (>0) g

-- 9.7
minval :: (Int -> Int) -> Int -> Int
minval f n = minimum x
    where x = map f [0..n]

alleq :: (Int -> Int) -> Int -> Bool
alleq f n = and iseqf_n
    where
        iseqf_n = map (==(f n)) all
        all = map f [0..n]

grz :: (Int -> Int) -> Int -> Bool
grz f n = greater (map f [0..n])

ascending :: (Int -> Int) -> Int -> Bool
ascending f n = and comp
    where
         comp = zipWith (<) x y 
            where 
                x = map f [0..(n-1)]
                y = map f [1..n]

-- 9.9
iter :: Int -> (a -> a) -> a -> a
iter 0 f x      = x
iter n f x      = iter (n-1) f (f x)

--9.10
double :: Int -> Int 
double n = 2*n

twopow :: Int -> Int
twopow n = iter (n-1) double 2

-- 9.11
smsqrs :: Int -> Int 
smsqrs n = foldr (+) 0 (map (^2) [0..n])

-- 9.16
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f [] = []
filterFirst f (x:xs)
    | f x = x : filterFirst f xs 
    | otherwise = xs

-- 9.17
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast f x = reverse (filterFirst f (reverse x))

-- 10.3
composeList :: [(a -> a)] -> (a -> a)
composeList [] = id 
composeList lst = foldr1 (.) lst 

-- 10.5
ins :: Int -> (Int -> Int)  
ins = (\n -> iter n succ)

test :: Int -> Int 
test n = f n 
    where
        f = ins n

-- 10.6
-- \x y -> f y x

-- 10.7
reord :: (a -> b -> c) -> (b -> a -> c)
reord f = (\x y -> f y x)

-- 10.8
nonwhite :: Char -> Bool
nonwhite c = not (f c) 
    where f = (\c -> elem c " \t\n")

-- 10.13
opsec :: [Int] -> [Int]
opsec lst = (map (+1) . filter (>(-1))) lst

-- 10.14
chessBoard :: Int -> [Char]
chessBoard n = helper 0 n

helper :: Int -> Int -> [Char]
helper i n
    | i == n        = ""
    | mod i 2 == 0  = whiterow 0 n ++ helper (i+1) n
    | otherwise     = blackrow 0 n ++ helper (i+1) n

whiterow :: Int -> Int -> [Char]
whiterow i n
    | i == n        = "\n"
    | mod i 2 == 0  = "  " ++ whiterow (i+1) n
    | otherwise     = "##" ++ whiterow (i+1) n

blackrow :: Int -> Int -> [Char]
blackrow i n
    | i == n        = "\n"
    | mod i 2 == 0  = "##" ++ blackrow (i+1) n
    | otherwise     = "  " ++ blackrow (i+1) n
