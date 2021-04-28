-- ilaese-8@student.ltu.se

module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read, write, repeat)
import Parser hiding (T, toString)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Skip |
    Read String | 
    Write Expr.T |
    Assignment String Expr.T |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Repeat Statement Expr.T
    deriving Show

-- Build skip statement on valid syntax { "skip;" }
skip = accept "skip" # require ";" >-> buildSkip
buildSkip s = Skip

-- Build read statement on valid syntax { "read count;"" }
read = accept "read" -# word #- require ";" >-> buildRead
buildRead r = Read r

-- Build write statement on valid syntax { "write expr;" }
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite w = Write w

-- Build assignment on valid syntax { "count := 0;" }
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- Build begin statement on valid syntax { "begin {statements} end" }
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin b = Begin b

-- Build if statement on valid syntax { "if x then skip; else x:=0-x;"" }
if_then = accept "if" -# Expr.parse # 
          require "then" -# parse #- 
          accept "else" # parse 
          >-> buildIf
buildIf ((e,s1),s2) = If e s1 s2

-- Build while statement on valid syntax { "while expr do {statement}" }
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s

-- Build repeat statement on valid syntax { "repeat {statement} until expr" }
repeat = (accept "repeat" -# parse #- require "until" # Expr.parse #- require ";") >-> buildRepeat
buildRepeat (s,e) = Repeat s e

-- Pattern matched execution functions
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Skip: stmts) dict input = exec stmts dict input 
exec (Read var: stmts) dict (x:xs) = exec stmts (Dictionary.insert (var, x) dict) xs
exec (Write expn: stmts) dict input = Expr.value expn dict : exec stmts dict input
exec (Assignment var val : stmts) dict input =
    exec stmts (Dictionary.insert (var, Expr.value val dict) dict) input 
exec (Begin beginStmts: stmts) dict input = exec (beginStmts ++ stmts) dict input
exec (While cond doStmt: stmts) dict input = 
    if (Expr.value cond dict)>0
    then exec (doStmt : While cond doStmt : stmts) dict input
    else exec stmts dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Repeat stmt cond : stmts) dict input =
    if (Expr.value cond dict)>0
    then exec stmts dict input
    else exec (stmt : (Repeat stmt cond) : stmts) dict input   

-- Statement toString function
toString :: T -> String
toString stmts = helper 0 stmts

-- toString helper function
helper :: Int -> T -> String
helper i (Skip) = indent i ++ "skip;\n"
helper i (Read str) = indent i ++ "read " ++ str ++ ";\n"
helper i (Write expn) = indent i ++ "write " ++ Expr.toString expn ++ ";\n"
helper i (Assignment str expn) = indent i ++ str ++ " := " ++ Expr.toString expn ++ ";\n"
helper i (Begin stmts) = indent i ++ "begin\n" ++ foldr (\x stmt -> helper (i+1) x ++ stmt) "" stmts ++ indent i ++ "end\n"
helper i (If expn s1 s2) = indent i ++ "if " ++ Expr.toString expn ++ " then\n" ++ helper (i+1) s1  ++ indent i ++ "else\n" ++ helper (i+1) s2 
helper i (While expn stmt) = indent i ++ "while " ++ Expr.toString expn ++ " do\n" ++ helper (i+1) stmt
helper i (Repeat stmt expn) = indent i ++ "repeat\n" ++ helper (i+1) stmt ++ "until " ++ Expr.toString expn ++ ";\n"

-- Returns n indententations
indent :: Int -> String
indent 0 = ""
indent n = " " ++ indent (n-1)

instance Parse Statement where
  parse = skip ! read ! write ! assignment ! begin ! if_then ! while ! repeat
  toString =  toString 