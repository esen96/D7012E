-- ilaese-8@student.ltu.se

import Prelude hiding (sum, tail) 

data IndexRange = IndexRange {
    from :: Int,
    to :: Int
} deriving (Show)

data Set = Set {
    setsum   :: Int,
    index    :: IndexRange,
    sublist  :: [Int]
} deriving (Show)

-- Entry function
smallestKsets :: [Int] -> Int -> IO ()
smallestKsets [] _  = error "Empty list"
smallestKsets lst k = putStrLn ( "\nEntire list: " ++ show lst ++ "\n\n" 
                                    ++ "size \t i \t j \t sublist \n" 
                                        ++ output sets 0 k)
    where sets = generateSets lst

-- Helper function that outputs the k smallest sets from a complete set
output :: [Set] -> Int -> Int -> [Char]
output (x:xs) count k
    | count == k    = ""
    | otherwise     = " " ++ show (setsum x) ++ "\t " ++ show (from (index x)) ++ "\t " ++ show (to (index x)) 
                        ++ "\t "  ++ show (sublist x) ++ "\n" ++ output xs (count+1) k

-- Generates all possible sets of a list in sorted sum order
generateSets :: [Int] -> [Set] 
generateSets lst = sorted (indexToSet lst indexList)
    where indexList = generateIndices (len lst)

-- Generates all possible index ranges of a list
generateIndices :: Int -> [IndexRange]
generateIndices range = [ IndexRange i j | 
                          i <- [1..range], 
                          j <- [1..range], 
                          i <= j          ]

-- Creates a list of sets from the input list and all possible indices
indexToSet :: [Int] -> [IndexRange] -> [Set]
indexToSet _ [] = []
indexToSet lst indices = Set (sum sub) (range) sub : indexToSet lst (tail indices)
    where
        range  = head indices
        i      = from range
        j      = to range
        sub    = subList lst 0 (i-1) (j-1)  

-- Creates a sublist of a list using from- and to indices
subList :: [Int] -> Int -> Int -> Int -> [Int]
subList [] _ _ _    = []
subList (x:xs) count from to
    | count > to    = []
    | count < from  = subList xs (count+1) from to
    | otherwise     = x : subList xs (count+1) from to

-- Sorts a list of sets based on their sums with the QuickSort algorithm
sorted :: [Set] -> [Set]
sorted [] = []
sorted (x:xs) = sorted [ y | y <- xs, setsum y <= setsum x] ++ [x] ++ sorted [y | y <- xs, setsum y > setsum x ]

-- Returns the length of a list
len :: [a] -> Int 
len []      = 0
len (x:xs)  = 1 + len xs

-- Calculates the sum of all numbers in a list
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- Gives the tail of a list
tail :: [a] -> [a]
tail (x:xs) = xs

-- Test cases
test1 = smallestKsets [x*(-1)^x | x <- [1..100]] 15
test2 = smallestKsets [24,-11,-34,42,-24,7,-19,21] 6
test3 = smallestKsets [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] 8