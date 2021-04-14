-- 12.2
numEqual :: Eq a => [a] -> a -> Int
numEqual [] _   = 0
numEqual (x:xs) item
    | item == x     = 1 + numEqual xs item
    | otherwise     = numEqual xs item

-- 12.3
olf :: Eq a => [ (a,b) ] -> a -> b
olf [] _    = error "No such item"
olf (x:xs) item
    | item == fst x     = snd x
    | otherwise         = olf xs item

ols :: Eq b => [ (a,b) ] -> b -> a
ols lst item = olf (map swap lst) item

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- 12.4
class Visible a where
  toString :: a -> String
  size     :: a -> Int

--instance Visible Int where
--    toString _ = "Not yet available"
--    size n     = 2*n

instance (Visible a, Visible b) => Visible (a,b) where
    toString (a,b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
    size _ = 2

instance (Visible a, Visible b, Visible c) => Visible (a,b,c) where
    toString (a,b,c) = "(" ++ toString a ++ "," ++ toString b ++ ","
                         ++ toString c ++ ")"
    size _ = 3

-- 12.5
instance Visible Int where 
    toString 0 = "0"
    toString 1 = "1"
    toString 2 = "2"
    toString 3 = "3"
    toString 4 = "4"
    toString 5 = "5"
    toString 6 = "6"
    toString 7 = "7"
    toString 8 = "8"
    toString 9 = "9"

    toString n 
        | n < 0     = "-" ++ toString (abs n)
        | otherwise = toString (div n 10) ++ toString (mod n 10)

    size n = n

-- 13.2
-- Yes, General unifier (Int,Bool)
{- No, (Int,a,a) limits (a,a,[Bool]) to (Int,Int,[Bool]),
   however, (a,a,[Bool]) limits (Int,a,a) to (Int,Bool,Bool).
   Hence they are mutually exclusive.
-} 


-- 13.3
-- a and b are both general types and therefore have a most general type of a
-- [a] and c are restricted to [a] since [a] is more restrictive than c
-- the most general type is therefore (a,[b]).
-- (Bool, [Bool]) is more specific case of the most general type

-- 13.4
-- Yes (Num,[Num]) -> b
-- Yes (Num, [Num]) -> b
-- No, since (a,[a]) implies the same types, whereas this scenario is (Num,[Bool])

-- 13.5
-- Yes (Num,[Num]) -> Num
-- Yes (Num, [Num]) -> Num
-- Still no, same reason
