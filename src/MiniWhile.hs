{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module MiniWhile
  ( parseEval,
    program,
    Program (..),
    Stmt (..),
    Exp (..),
    Aexp (..),
    Op(..),
    CmpOp(..),
    EvalError(..),
    languageDef
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State
import Data.Functor (($>))
import qualified Data.Functor.Identity
import qualified Data.Map as M
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import TextShow
import TextShow.Data.Char (showbLitString)

newtype Program = Program [Stmt] deriving (Show, Eq)

data Stmt
  = Asgn Id Exp
  | While Exp [Stmt]
  deriving (Show, Eq)

data CmpOp
  = TLE
  | TGT
  | TEQ
  | TNEQ
  deriving (Show, Eq)

data Exp
  = If Exp Exp Exp
  | Cmp CmpOp Aexp Aexp
  | Not Exp
  | Aexp Aexp
  deriving (Show, Eq)

data Op
  = TAdd
  | TMult
  | TDiv
  | TSub
  deriving (Show, Eq)

data Aexp
  = Num Integer
  | Var Id
  | Brk Op Aexp Aexp
  deriving (Show, Eq)

type Id = String

instance TextShow Op where
  showb :: Op -> Builder
  showb TAdd = " + "
  showb TMult = " * "
  showb TDiv = " / "
  showb TSub = " - "

instance TextShow Aexp where
  showb :: Aexp -> Builder
  showb (Num x) = showb x
  showb (Var x) = showbLitString x
  showb (Brk op ae1 ae2) = " (" <> showb ae1 <> showb op <> showb ae2 <> ") "

instance TextShow CmpOp where
  showb :: CmpOp -> Builder
  showb TLE = " <= "
  showb TGT = " > "
  showb TEQ = " == "
  showb TNEQ = " != "

instance TextShow Exp where
  showb :: Exp -> Builder
  showb (If e1 e2 e3) = "if " <> showb e1 <> " then " <> showb e2 <> " else " <> showb e3 <> " fi "
  showb (Cmp op ae1 ae2) = showb ae1 <> showb op <> showb ae2
  showb (Not e) = "not " <> showb e
  showb (Aexp ae) = showb ae

unwordsSepBy :: Char -> [Builder] -> Builder
unwordsSepBy c (b:bs@(_:_)) = b <> singleton c <> unwordsSepBy c bs
unwordsSepBy _ [b] = b
unwordsSepBy _ [] = mempty

instance TextShow Stmt where
  showb :: Stmt -> Builder
  showb (Asgn x e) = showbLitString x <> " := " <> showb e
  showb (While e stmts) = "while " <> showb e <> " do " <> unwordsSepBy ';' (showb <$> stmts) <> " done "

instance TextShow Program where
  showb :: Program -> Builder
  showb (Program stmts) = unwordsSepBy ';' (showb <$> stmts)

languageDef :: Token.LanguageDef u
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames =
        [ "if",
          "then",
          "else",
          "while",
          "do",
          "true",
          "false",
          "not",
          "and",
          "or"
        ],
      Token.reservedOpNames =
        [ "+",
          "-",
          "*",
          "/",
          ":=",
          "==",
          "<=",
          ">",
          "and",
          "or",
          "not"
        ]
    }

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer -- parses an identifier

reserved :: String -> Parser ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

parens :: Parser a -> Parser a
parens = Token.parens lexer -- parses surrounding parenthesis:

integer :: Parser Integer
integer = Token.integer lexer -- parses an integer

semi :: Parser String
semi = Token.semi lexer -- parses a semicolon

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

asgn :: Parser Stmt
asgn = do
  var <- identifier
  reservedOp ":="
  Asgn var <$> pexp

while :: Parser Stmt
while = do
  reserved "while"
  cond <- pexp
  reserved "do"
  stmts <- stmt `sepBy1` semi
  reserved "done"
  return $ While cond stmts

ifStmt :: Parser Exp
ifStmt = do
  reserved "if"
  exp1 <- pexp
  reserved "then"
  exp2 <- pexp
  reserved "else"
  exp3 <- pexp
  reserved "fi"
  return $ If exp1 exp2 exp3

aexp :: Parser Aexp
aexp = aTerm `chainl1` aMult `chainl1` aAdd

aAdd :: Parser (Aexp -> Aexp -> Aexp)
aAdd = reservedOp "+" $> Brk TAdd <|> reservedOp "-" $> Brk TSub

aMult :: Parser (Aexp -> Aexp -> Aexp)
aMult = reservedOp "*" $> Brk TMult <|> reservedOp "/" $> Brk TDiv

aTerm :: Parser Aexp
aTerm =
  parens aexp
    <|> Var <$> identifier
    <|> Num <$> integer

pexp :: Parser Exp
pexp = ifStmt <|> try pCmp <|> notExp <|> Aexp <$> aexp

notExp :: Parser Exp
notExp = reserved "not" >> Not <$> pexp

pCmp :: Parser Exp
pCmp = do
  aexp1 <- aexp
  cmp <-
    (reservedOp "<=" >> return TLE)
      <|> (reservedOp "==" >> return TEQ)
      <|> (reservedOp "!=" >> return TNEQ)
      <|> (reservedOp ">" >> return TGT)
  Cmp cmp aexp1 <$> aexp

stmt :: Parser Stmt
stmt = while <|> asgn

program :: Parser Program
program = do
  whiteSpace
  lStmt <- do
    x <- stmt
    xs <- try (semi >> eof >> return []) <|> many (semi >> stmt)
    return (x:xs)
  eof
  return $ Program lStmt

type Env = M.Map Id Integer

data EvalError
  = ParseError ParseError
  | VariableNotDefined
  | IntegerInBoolean
  | BooleanInInteger
  deriving (Eq, Show)

type Evaluator = ExceptT EvalError (StateT Env IO)

evalStmt :: Stmt -> Evaluator ()
evalStmt (Asgn i e) = do
  x <- evalExp e
  modify $ M.insert i x
evalStmt w@(While prop stmts) = do
  mapM_ evalStmt stmts
  propTrue <- evalProp prop
  when propTrue $ evalStmt w

evalProp :: Exp -> Evaluator Bool
evalProp (Cmp op e1 e2) = do
  x <- evalAexp e1
  y <- evalAexp e2
  case op of
    TLE -> return $ x <= y
    TGT -> return $ x > y
    TEQ -> return $ x == y
    TNEQ -> return $ x /= y
evalProp (Not prop) = do
  p <- evalProp prop
  return $ not p
evalProp _ = throwError IntegerInBoolean

evalExp :: Exp -> Evaluator Integer
evalExp (If prop e1 e2) = do
  propTrue <- evalProp prop
  if propTrue then evalExp e1 else evalExp e2
evalExp (Aexp e) = evalAexp e
evalExp _ = throwError BooleanInInteger

evalAexp :: Aexp -> Evaluator Integer
evalAexp (Num x) = return x
evalAexp (Var v) = do
  env <- get
  let x = M.lookup v env
  case x of
    Nothing -> throwError VariableNotDefined
    Just n -> return n
evalAexp (Brk op ae1 ae2) = do
  x <- evalAexp ae1
  y <- evalAexp ae2
  case op of
    TAdd -> return $ x + y
    TMult -> return $ x * y
    TDiv -> return $ x `div` y
    TSub -> return $ x - y

eval :: Program -> Evaluator Env
eval p@(Program stmts) = do
  liftIO $ printT p
  mapM_ evalStmt stmts
  get

parseEval :: String -> IO (Either EvalError Env)
parseEval s =
  let x = parse program "" s
   in case x of
        Left parseError -> return $ Left (ParseError parseError)
        Right prg -> do
          (e, _) <- runStateT (runExceptT (eval prg)) M.empty
          case e of
            err@(Left _) -> return err
            Right env -> return $ Right env
