module ParseCompy where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data BBinOp = BoolConst Bool
            | Not BExpr
            | BBinary BBinOp BExpr BExpr
            | RBinary RBinOp AExpr AExpr
            deriving (Show)

data RBinOp = And | Or
            deriving (Show)

data RBinOp = Greater 
            | Less 
            | GreaterEq 
            | LessEq 
            | Equal 
            | NotEq
            deriving (Show)

data AExpr = Var String 
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = Add 
            | Subtract
            | Multiply
            | Divide
            | Module 
            deriving (Show) 

data Statement = Assign String AExpr
               | If BExpr Statement [(BExpr, Statement)] Statement
               | While BExpr Statement
               | Block [Statement]
               | Skip
               deriving (Show)

languageDef = 
    emptyDef {
        Token.commentStart = "\"\"\"",
        Token.commentEnd = "\"\"\"",
        Token.commentLine = "#",
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = [
            "if", "elif", "else", "while", "skip", "true", "false"
        ],
        Token.reservedOpNames = [
            "=", ">", "<", ">=", "<=", "!=", "==", "*", "/", "%", "+", "-", "and", "or"
        ]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Statement
whileParser = whiteSpace >> statement

statement :: Parser Statement
statement = parens statement
        <|> sequenceOfStatement

sequenceOfStatement =
    do list <- (sepBy1 statement' semi)
       return $ if length list == 1 
            then head list 
            else Block list

statement' :: Parser Statement
statement' = ifStmt 
        <|> whileStmt 
        <|> assignStmt 
        <|> skipStmt

ifStmt :: Parser Statement
ifStmt = do reserved "if"
            cond <- bExpression
            reserved ":"
            stmt <- statement
            elifs <- many elifStmt
            reserved "else"
            elif <- bExpression
            reserved ":"
            elstmt <- statement
            return $ If cond stmt elifs elstmt
    where 
        elifStmt = do reserved "elif"
                      cond <- bExpression
                      reserved ":"
                      stmt <- statement
                      return (cond, stmt)

whileStmt :: Parser Statement
whileStmt = do reserved "while"
               cond <- bExpression
               reserved ":"
               stmt <- statement
               return $ While cond stmt

assignStmt :: Parser Statement
assignStmt = do var <- identifier
                reservedOp "="
                expr <- aExpression
                return $ Assign var expr

skipStmt :: Parser Statement
skipStmt = reserved "pass" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-" >> return (Neg))]
             , [Infix (reservedOp "*" >> return (ABinary Multiply)) AssocLeft]
             , [Infix (reservedOp "/" >> return (ABinary Divide)) AssocLeft]
             , [Infix (reservedOp "%" >> return (ABinary Module)) AssocLeft]
             , [Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft]
             , [Infix (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not))]
             , [Infix (reservedOp "and" >> return (BBinary And)) AssocLeft]
             , [Infix (reservedOp "or" >> return (BBinary Or)) AssocLeft]
             ]

aTerm = parens aExpression
    <|> liftM Var identifier
    <|> liftM IntConst integer

bTerm = parens bExpression
    <|> (reserved "True" >> return (BoolConst True))
    <|> (reserved "False" >> return (BoolConst False))
    <|> rExpression

rExpression = 
    do a1 <- aExpression
       op <- relation
       a2 <- aExpression
       return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less)
        <|> (reservedOp ">=" >> return GreaterEq)
        <|> (reservedOp "<=" >> return LessEq)
        <|> (reservedOp "!=" >> return NotEq)
        <|> (reservedOp "==" >> return Equal)

parseString :: String -> Statement
parseString str = 
    case parse whileParser "" str of
        Left e -> error $ show e
        Right r -> r

parseFile :: String -> IO Statement
parseFile file = 
    do program <- readFile file
       case parse whileParser "" program of
           Left e -> print e >> fail "parse error"
           Right r -> return r