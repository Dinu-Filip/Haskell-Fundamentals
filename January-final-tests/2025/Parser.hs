module Parser where

import Types
import Lexer
import Examples

import Data.Maybe
import Control.Monad

------------------------------------------------------------------------------
-- Given...

showToken :: Token -> String
showToken (Ident v) = v
showToken (Nat v) = show v
showToken WhileTok = "while"
showToken t = [head [c | (c, t') <- tokenTable, t == t']]

printParse :: String -> IO ()
printParse input = either printError printOK (parse input)
  where
    printOK prog = putStrLn "Parse successful..." >> print prog
    printError err = putStr "Parse error: " >> printError' err
    printError'' t s = putStrLn (s ++ " expected, but " ++
                                 maybe "nothing" showToken t ++ " found")
    printError' (BadChar c) = do putStr "Unrecognised character: "
                                 putStrLn [c]
    printError' (Unexpected t t') = printError'' t (showToken t')
    printError' (StmtNotFound t) = printError'' t "Statement"
    printError' (ExprNotFound t) = printError'' t "Expression"
    printError' (IntNotFound t) = printError'' t "Integer literal"
    printError' (UnparsedInput toks) = putStrLn ("Unparsed input: " ++
                                                 unwords (map showToken toks))

------------------------------------------------------------------------------
-- Part I

-- Given...
mHead :: [a] -> Maybe a
mHead (x : _) = Just x
mHead _ = Nothing

checkTok :: Token -> [Token] -> Either Error [Token]
checkTok t (t' : ts)
  | t == t'   = Right ts
checkTok t ts = Left (Unexpected (mHead ts) t)

parseAtom :: Parser Expr
parseAtom (Minus : Nat x : ts) = Right (ts, Val (negate x))
parseAtom (Minus : ts)         = Left (IntNotFound (mHead ts))
parseAtom (Ident v : ts)       = Right (ts, Var v)
parseAtom (Nat n : ts)         = Right (ts, Val n)
parseAtom (LParen : ts)        = do
  (pRem, atom) <- parseExpr ts
  pRem'        <- checkTok RParen pRem
  pure (pRem', atom)
parseAtom ts                   = Left (ExprNotFound (mHead ts))

parseStmtAssign :: Parser Expr 
                -> Token 
                -> (Expr -> Expr -> Expr)
                -> Parser Expr
parseStmtAssign subParse tCons eCons ts = do
  (pRem, e) <- subParse ts
  parseStmtAssign' e pRem

  where parseStmtAssign' :: Expr -> Parser Expr
        parseStmtAssign' acc (t : ts)
          | t == tCons = do
              (toks', x) <- subParse ts
              parseStmtAssign' (eCons acc x) toks'
        parseStmtAssign' acc toks = Right (toks, acc)

parseTerm :: Parser Expr
parseTerm = parseStmtAssign parseAtom Times Mul

parseExpr :: Parser Expr
parseExpr = parseStmtAssign parseTerm Plus Add

parseStmt :: Parser Stmt
parseStmt (Ident v : Eq : ts) 
  = do
    (pRem, e) <- parseExpr ts
    pure (pRem, Asgn v e)
parseStmt (WhileTok : ts) 
  = do
    -- Parse expression first and check followed by left brace
    (epRem, e) <- parseExpr ts
    epRem'     <- checkTok LBrace epRem
    -- If token remainder of expression is non-null, then parsing body will
    -- fail
    (bpRem, b) <- parseBlock epRem'
    bpRem'     <- checkTok RBrace bpRem
    pure (bpRem', While e b)
parseStmt s 
  = Left (StmtNotFound (mHead s))

parseBlock :: Parser Block
parseBlock b = do
  (pRem, stmt) <- parseStmt b
  parseBlock' [stmt] pRem

  where parseBlock' :: [Stmt] -> Parser Block
        parseBlock' stmts (Semi : e) = do
          (toks', x) <- parseStmt e
          parseBlock' (x : stmts) toks'
        -- Single reverse since statements accumulated in reverse order
        parseBlock' stmts b          = Right (b, reverse stmts)

parse :: String -> Either Error Program
parse input = do
  input'    <- tokenise input
  (pRem, b) <- parseBlock input'
  case pRem of
    [] -> pure b
    _  -> Left (UnparsedInput pRem)
