module Denotational where

import           Control.Applicative hiding (many, some)
import           Control.Monad ((>=>))
import           Data.Function (fix)
import           Data.Void
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type ParsecError = (ParseErrorBundle String Void)

parseAndEvalProgram :: String
                    -> Either ParsecError Sigma
parseAndEvalProgram input = do
  c <- parse comExp "program" input
  denoteM c (Sigma M.empty)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

aExp :: Parser AExp
aExp = makeExprParser aTerm aOperators

aOperators :: [[Operator Parser AExp]]
aOperators =
  [[  InfixL (Mult <$ symbol "*")
    , InfixL (Sub <$ symbol "-") ]
  ]

aTerm :: Parser AExp
aTerm =
      parens aExp
  <|> Var <$> identifier
  <|> Val . fromIntegral <$> integer

bExp :: Parser BExp
bExp = makeExprParser bTerm bOperators

prefix :: String -> (BExp -> BExp) -> Operator Parser BExp
prefix name f = Prefix  (f <$ symbol name)

bOperators :: [[Operator Parser BExp]]
bOperators =
  [[ InfixL (And <$ symbol "&&") ]]

bTerm :: Parser BExp
bTerm =
      parens bExp
  <|> True' <$ symbol "true"
  <|> symbol "not" *> bExp
  <|> (do
        a1 <- aExp
        symbol "<="
        a2 <- aExp
        return $ LT' a1 a2)

comExp :: Parser Command
comExp = makeExprParser comTerm comOperators

comOperators :: [[Operator Parser Command]]
comOperators =
  [[ InfixL (Seq <$ symbol ";") ]]

comTerm :: Parser Command
comTerm =
      parens comExp
  <|> Skip <$ symbol "skip"
  <|> Ass <$> identifier
          <*> (symbol ":=" *> aExp)
  <|> If <$> (symbol "if" *> bExp)
         <*> (symbol "then" *> comExp)
         <*> (symbol "else" *> comExp)
  <|> While <$> (symbol "while" *> bExp)
            <*> (symbol "do" *> comExp)

-- | MAPPING OF EVALUATION ACCORDING TO DENOTATIONAL SEMANTICS

-- | state of the while program
newtype Sigma = Sigma (M.Map String Int)
  deriving (Eq, Show)

-- | arithmetic expressions
data AExp =
    Val Int
  | Var String
  | Sub  AExp AExp
  | Mult AExp AExp
  deriving (Eq, Read, Show)

evalAExp :: AExp -> Sigma -> Int
evalAExp (Val v) _ = v
evalAExp (Var x) (Sigma s)
  = maybe 0 id $ M.lookup x s
evalAExp (Sub a1 a2) s
  = (evalAExp a1 s) - (evalAExp a2 s)
evalAExp (Mult a1 a2) s
  = (evalAExp a1 s) - (evalAExp a2 s)

-- | boolean expressions
data BExp =
    True'
  | LT' AExp AExp
  | Not BExp
  | And BExp BExp
  deriving (Eq, Read, Show)

evalBExp :: BExp -> Sigma -> Bool
evalBExp True' _ = True
evalBExp (LT' a1 a2) s =
  (evalAExp a1 s) <= (evalAExp a2 s)
evalBExp (Not b) s = not $ evalBExp b s
evalBExp (And b1 b2) s =
  (evalBExp b1 s) && (evalBExp b2 s)

-- | commands in the while language
-- without syntactic sugar for now
data Command =
    Skip
  | Ass String AExp
  | Seq Command Command
  | If BExp Command Command
  | While BExp Command
  deriving (Eq, Read, Show)

-- | IF functional
if' :: Monad m
     => (Sigma -> Bool)
     -> (Sigma -> m Sigma)
     -> (Sigma -> m Sigma)
     -> Sigma
     -> m Sigma
if' p f g =
  \(Sigma s) -> case p (Sigma s) of
    True -> f $ Sigma s
    False -> g $ Sigma s

-- | denote function
denoteM :: Monad m
        => Command
        -> Sigma
        -> m Sigma
denoteM Skip = pure
denoteM (Ass x a) =
  pure . (\s'@(Sigma s) ->
            let i = evalAExp a s'
            in Sigma $ M.insert x i s)
denoteM (Seq c1 c2) =
  denoteM c1 >=> denoteM c2
denoteM (If b c1 c2) =
  if' (evalBExp b) (denoteM c1) (denoteM c2)
denoteM (While b c) = -- TODO: write my own `fix`
  fix (\f ->
          if' (evalBExp b) (denoteM c >=> f) pure)
