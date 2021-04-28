-- ilaese-8@student.ltu.se

module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a,b) = a:b

-- Combined parser excluding parser 1 result
(-#) :: Parser a -> Parser b -> Parser b
(-#) pa pb = pa # pb >-> snd 

-- Combined parser excluding parser 2 result
(#-) :: Parser a -> Parser b -> Parser a
(#-) pa pb = pa # pb >-> fst

-- White space parser
spaces :: Parser String
spaces =  iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- Letter parser
letter :: Parser Char
letter =  char ? isAlpha

-- Word separator parser
word :: Parser String
word = token (letter # iter letter >-> cons)

-- n chars parser
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

-- Accepting specific substring parser
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Requiring specific substring parser
require :: String -> Parser String
require w = accept w ! err ("expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

