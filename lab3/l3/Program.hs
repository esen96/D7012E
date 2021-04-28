-- ilaese-8@student.ltu.se

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show 

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = toString'

-- toString every statement in the program and return it as a String
toString' :: T -> String
toString' (Program stmts) = foldr (\x y -> Statement.toString x ++ y) "" stmts

exec (Program p) = Statement.exec p Dictionary.empty
