-- 3.7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = (m/=n) && (n/=p) && (m/=p)

-- 3.8
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (n==p)

--fourEqual :: Int -> Int -> Int -> Int -> Bool
--fourEqual m n o p = (m==n) && (n==o) && (o==p)

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n o p = (threeEqual m n o) && (o==p)

-- 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
    | b^2 > (4.0*a*c)   = 2
    | b^2 == (4.0*a*c)  = 1
    | otherwise         = 0

-- 3.16
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
    | (a==0.0) && (b==0.0) && (c==0.0) = 3
    | otherwise = numberNDroots a b c

-- 3.17
smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
    | b^2-(4*a*c) > 0 && (a /= 0) = minRoot a b c
    | otherwise                   = 0

largerRoot a b c
    | b^2-(4*a*c) > 0 && (a /= 0) = maxRoot a b c
    | otherwise                   = 0
 
minRoot :: Float -> Float -> Float -> Float
minRoot a b c = min ((-b)+sqrt(b^2-(4*a*c)) / (2*a)) ((-b)-sqrt(b^2-(4*a*c)) / (2*a))

maxRoot :: Float -> Float -> Float -> Float
maxRoot a b c = max ((-b)+sqrt(b^2-(4*a*c)) / (2*a)) ((-b)-sqrt(b^2-(4*a*c)) / (2*a))

-- 4.7
mult :: Int -> Int -> Int
mult a b
    | b == 0    = 0
    | otherwise = a + mult a (b-1)

-- 4.8
intSqrt :: Int -> Int 
intSqrt n 
    | n <= 0    = 0
    | otherwise = helper n 1

helper :: Int -> Int -> Int 
helper a b
    | b^2 > a   = (b-1)
    | otherwise = helper a (b+1)

-- 4.9
f :: Int -> Int 
f 0 = 0
f 1 = 44
f 2 = 17
f 3 = 55
f 4 = 11
f 5 = 88
f _ = 0

maxF :: Int -> Int
maxF n
    | n == 0 = f n
    | otherwise = f n `max` maxF (n-1)

-- 4.14
twoPow :: Int -> Int 
twoPow n 
    | div n 2 == 1  = 2^n
    | mod n 2 == 0  = twoPow (div n 2) * twoPow (div n 2)
    | otherwise     = twoPow (div n 2) * twoPow (div n 2)*2
