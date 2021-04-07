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


