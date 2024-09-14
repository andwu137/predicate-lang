module Predicate.Parser (
    Identifier,
    Statement (..),
    Expr (..),
    Dir2D (..),
    testParser,
    statements,
    statement,
    expr,
    apply,
) where

import Control.Monad
import Control.Monad.Combinators
import Data.Function
import Data.Functor
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Debug as MP
import Text.Parser.Combinators

{- Parser -}
type Parser = MP.Parsec Void T.Text

testParser :: Parser a -> T.Text -> Either String a
testParser p test = do
    MP.parse p "source-file" test & \case
        Left bundle -> Left $ MP.errorBundlePretty bundle
        Right s -> Right s

{- Constants -}
maxTerm :: Int
maxTerm = 2

deckIdentifier :: String
deckIdentifier = "deck"

{- Data Types -}
type Identifier = String
type CardId = String
type Deck = [(CardId, Integer)]

data Statement
    = Assign Identifier [Identifier] Expr
    | SetDeck Deck
    deriving (Show, Eq)

data Expr
    = Card CardId
    | Num Integer
    | Ident Identifier
    | Not Expr
    | Or Expr Expr
    | And Expr Expr
    | Implies Expr Expr
    | Apply Expr [Expr]
    deriving (Show, Eq)

data Dir2D = Backwards | Forwards
    deriving (Show, Eq)

{- Parse Functions -}
bracket :: Parser a -> Parser a
bracket p = MP.char '(' *> p <* MP.char ')'

bracketable :: Parser a -> Parser a
bracketable p = bracket p <|> p

bracketHSpaceable :: Parser a -> Parser a
bracketHSpaceable p = bracket (surroundHSpace p) <|> p

surroundHSpace :: Parser a -> Parser a
surroundHSpace = MP.try . surround MP.hspace

statements :: Parser [Statement]
statements = many (surround MP.space statement) <* MP.eof

nextLine :: Parser ()
nextLine = MP.hspace *> MP.newline *> MP.space

-- >>> testParser statement "function = x | y & z;"
-- >>> testParser statement "function = do\nx | y & z\na & b & c\n;"
-- Right (Assign "function" (And (Or (Ident "x") (Ident "y")) (Ident "z")))
-- Left "source-file:3:1:\n  |\n3 | a & b & c\n  | ^\nunexpected 'a'\nexpecting \"->\", '&', ';', '|', or white space\n"
statement :: Parser Statement
statement = do
    n <- identifier
    if n == deckIdentifier
        then do
            equalSign
            SetDeck <$> deck
        else do
            ps <- MP.hspace *> params
            equalSign
            e <- doNotation <|> expr
            MP.space <* MP.char ';'
            pure $ Assign n ps e

params :: Parser [String]
params = identifier `sepEndBy` MP.hspace

equalSign :: Parser ()
equalSign = void $ surround MP.space $ MP.char '='

deck :: Parser Deck
deck = do
    MP.char '[' *> MP.space
    xs <- sepEndBy1 (tuple card num) (MP.space *> MP.char ',' <* MP.space)
    MP.space <* MP.char ']'
    pure xs

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple px py = do
    MP.char '(' *> MP.space
    x <- px <* MP.space
    void $ MP.char ','
    y <- MP.space *> py
    MP.space <* MP.char ')'
    pure (x, y)

doNotation :: Parser Expr
doNotation = do
    MP.string "do" *> MP.space1
    fmap (foldl1 And) (expr `sepBy1` nextLine)

-- >>> testParser identifier "function"
-- >>> testParser identifier "!function"
-- Right "function"
-- Left "source-file:1:1:\n  |\n1 | !function\n  | ^\nunexpected '!'\nexpecting letter\n"
identifier :: Parser Identifier
identifier =
    MP.letterChar <:> many (MP.alphaNumChar <|> MP.oneOf special)
  where
    special = "_'." :: String

-- >>> testParser expr "e1"
-- >>> testParser expr "e1 | e2 & e3"
-- >>> testParser expr "e1 & ~e2 | e3"
-- >>> testParser expr "e1 & ( e2 | ~e3 )"
-- >>> testParser expr "~e1 -> e2 & e3"
-- >>> testParser expr "e1 | e2 -> e3 & e4"
-- >>> testParser expr "(e1 | e2) -> e3 & ~e4"
-- >>> testParser expr "e1 -> e2 -> e3"
-- Right (Ident "e1")
-- Right (And (Or (Ident "e1") (Ident "e2")) (Ident "e3"))
-- Right (Or (And (Ident "e1") (Not (Ident "e2"))) (Ident "e3"))
-- Right (And (Ident "e1") (Or (Ident "e2") (Not (Ident "e3"))))
-- Right (Implies (Not (Ident "e1")) (And (Ident "e2") (Ident "e3")))
-- Right (Implies (Or (Ident "e1") (Ident "e2")) (And (Ident "e3") (Ident "e4")))
-- Right (Implies (Or (Ident "e1") (Ident "e2")) (And (Ident "e3") (Not (Ident "e4"))))
-- Right (Implies (Ident "e1") (Implies (Ident "e2") (Ident "e3")))
expr :: Parser Expr
expr =
    surroundHSpace term >>= expr' maxTerm
  where
    expr' n l =
        ( do
            (m, dir, o) <- surroundHSpace $ binOps n
            case dir of
                Forwards ->
                    o l <$> (term >>= expr' m)
                Backwards -> do
                    r <- term
                    expr' m (o l r)
        )
            <|> if n >= maxTerm
                then pure l
                else expr' (n + 1) l

term :: Parser Expr
term =
    ((unOps <* MP.hspace) >>= (<$> term))
        <|> choice
            [ bracket expr
            , exprIdent
            , bracketable apply
            ]

{- Nullary Expr -}
exprIdent :: Parser Expr
exprIdent = bracketHSpaceable $ Ident <$> identifier

{- Unary Expr -}
unOps :: Parser (Expr -> Expr)
unOps = choice [exprNot]

exprNot :: Parser (Expr -> Expr)
exprNot = MP.char '~' $> Not

{- Binary Expr -}
binOps :: Int -> Parser (Int, Dir2D, Expr -> Expr -> Expr)
binOps n =
    choice
        . concat
        . zipWith (\i -> fmap (\(d, e) -> (i,d,) <$> e)) [0 ..]
        . take n
        $ [ [(Forwards, exprImpl)]
          ,
              [ (Backwards, exprOr)
              , (Backwards, exprAnd)
              ]
          ]

exprOr, exprAnd, exprImpl :: Parser (Expr -> Expr -> Expr)
exprOr = MP.char '|' $> Or
exprAnd = MP.char '&' $> And
exprImpl = MP.string "->" $> Implies

-- >>> testParser apply "{a #2 #1 2}"
-- >>> testParser apply "{a ({has 2} | {give 3})}"
-- >>> testParser apply "{a}"
-- Right (Apply "a" [Card "2",Card "1",Ident "2"])
-- Right (Apply "a" [Or (Apply "has" [Ident "2"]) (Apply "give" [Ident "3"])])
-- Left "source-file:1:3:\n  |\n1 | {a}\n  |   ^\nunexpected '}'\nexpecting '#', '(', alphanumeric character, digit, or white space\n"
apply :: Parser Expr
apply = do
    MP.char '{' *> MP.hspace
    i <- term
    MP.space
    a <- arg `sepBy` MP.hspace1
    MP.space <* MP.char '}'
    pure $ Apply i a

arg :: Parser Expr
arg =
    choice
        [ Card <$> card
        , Num <$> num
        , term
        ]

num :: (Read a) => Parser a
num = read <$> some MP.digitChar

card :: Parser CardId
card = MP.char '#' *> some MP.digitChar
