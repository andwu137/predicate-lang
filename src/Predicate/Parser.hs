{- example
deck =
    [ (#1, 2)
    , (#2, 1)
    , (#3, 3)
    , (#4, 1)
    ...
    ];

main =
    [ func1 > func2
    , func1 & lt 2 #4
    ];

func1 = has 3 #1 | has 1 #2;

func2 = not (has 3 #3);
 -}

module Predicate.Parser (

) where

import Control.Applicative (Alternative ())
import Control.Arrow
import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

testParser :: Parser a -> T.Text -> Either String a
testParser p test =
    case parse p "source-file" test of
        Left bundle -> Left $ errorBundlePretty bundle
        Right s -> Right s

type Identifier = String
data Stmt = Assign Identifier Expr
    deriving (Show, Eq)
data Expr
    = Or Expr Expr
    | And Expr Expr
    | Implies Expr Expr
    | Ident Identifier
    deriving (Show, Eq)

function :: Parser (String, Expr)
function = do
    n <- name
    void $ space *> char '=' *> space
    e <- expr
    pure (n, e)

-- >>> testParser name "function"
-- >>> testParser name "!function"
-- Right "function"
-- Left "source-file:1:1:\n  |\n1 | !function\n  | ^\nunexpected '!'\nexpecting alphanumeric character or letter\n"
name :: Parser Identifier
name = (maybeToList <$> optional letterChar) <> some alphaNumChar

chainl1 :: (Monad m, Alternative m) => m a -> m (a -> a -> a) -> m a
chainl1 p = chainl p p

chainl :: (Monad m, Alternative m) => m a -> m a -> m (a -> a -> a) -> m a
chainl p1 p o = p1 >>= rest
  where
    rest l =
        (($ l) <$> o <*> p >>= rest)
            <|> pure l

chainr1 :: (Monad m, Alternative m) => m a -> m (a -> a -> a) -> m a
chainr1 p = chainr p p

chainr :: (Monad m, Alternative m) => m a -> m a -> m (a -> a -> a) -> m a
chainr p1 p o = p1 >>= rest
  where
    rest l =
        (($ l) <$> o <*> (p >>= rest))
            <|> pure l

-- >>> testParser expr "e1"
-- >>> testParser expr "e1 | e2 & e3"
-- >>> testParser expr "e1 & e2 | e3"
-- >>> testParser expr "e1 -> e2 & e3"
-- >>> testParser expr "e1 | e2 -> e3 & e4"
-- >>> testParser expr "e1 -> e2 -> e3"
-- Right (Ident "e1")
-- Right (And (Or (Ident "e1") (Ident "e2")) (Ident "e3"))
-- Right (Or (And (Ident "e1") (Ident "e2")) (Ident "e3"))
-- Right (Implies (Ident "e1") (And (Ident "e2") (Ident "e3")))
-- Right (Or (Ident "e1") (Implies (Ident "e2") (And (Ident "e3") (Ident "e4"))))
-- Right (Implies (Ident "e1") (Implies (Ident "e2") (Ident "e3")))
expr :: Parser Expr
expr =
    chainr exprIdent expr exprImpl
        `chainl1` (exprOr <|> exprAnd)
  where
    exprIdent = Ident <$> (space *> name <* space)
    ex s f = string s $> f
    exprOr = ex "|" Or
    exprAnd = ex "&" And
    exprImpl = ex "->" Implies
