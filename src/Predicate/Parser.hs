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
    identifier,
    surroundHSpace,
    bracket,
    bracketable,
) where

import Control.Monad
import Control.Monad.Combinators
import Data.Function
import Data.Functor
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Text.Parser.Combinators

{- Parser -}
type Parser = MP.Parsec Void T.Text

testParser :: Parser a -> T.Text -> Either String a
testParser p test = do
    MP.parse p "source-file" test & \case
        Left bundle -> Left $ MP.errorBundlePretty bundle
        Right s -> Right s

{- Data Types -}
type Identifier = String
type CardId = String
type Deck = [(CardId, Integer)]

data Statement
    = Assign Identifier Expr
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
    | Apply Identifier [Expr]
    deriving (Show, Eq)

data Dir2D = Backwards | Forwards
    deriving (Show, Eq)

{- Parse Functions -}
maxTerm :: Int
maxTerm = 2

bracket :: Parser a -> Parser a
bracket p = MP.char '(' *> p <* MP.char ')'

bracketable :: Parser a -> Parser a
bracketable p = bracket p <|> p

surroundHSpace :: Parser a -> Parser a
surroundHSpace = MP.try . surround MP.hspace

statements :: Parser [Statement]
statements = many (surround MP.space statement) <* MP.eof

-- >>> testParser statement "function = x | y & z;"
-- >>> testParser statement "function = do\nx | y & z\na & b & c\n;"
-- Right (Assign "function" (And (Or (Ident "x") (Ident "y")) (Ident "z")))
-- Left "source-file:3:1:\n  |\n3 | a & b & c\n  | ^\nunexpected 'a'\nexpecting \"->\", '&', ';', '|', or white space\n"
statement :: Parser Statement
statement = do
    n <- identifier
    void $ surround MP.space $ MP.char '='
    case n of
        "deck" -> SetDeck <$> deck
        _ -> do
            e <- doNotation <|> expr
            MP.space <* MP.char ';'
            pure $ Assign n e

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
    fmap (foldl1 And) (sepBy1 expr MP.space1)

-- >>> testParser identifier "function"
-- >>> testParser identifier "!function"
-- Right "function"
-- Left "source-file:1:1:\n  |\n1 | !function\n  | ^\nunexpected '!'\nexpecting letter\n"
identifier :: Parser Identifier
identifier = MP.letterChar <:> many MP.alphaNumChar

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
expr = term >>= expr' maxTerm
  where
    expr' n l =
        ( do
            (m, dir, o) <- choice $ binOps n
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

    term =
        (surroundHSpace unOps >>= (<$> term))
            <|> surroundHSpace
                ( choice
                    [ bracket expr
                    , bracketable exprIdent
                    , bracketable apply
                    ]
                )

    unOps = choice [exprNot]
    binOps n =
        concat
            . zipWith (\i -> fmap (\(d, e) -> (i,d,) <$> e)) [0 ..]
            . take n
            $ [ [(Forwards, exprImpl)]
              ,
                  [ (Backwards, exprOr)
                  , (Backwards, exprAnd)
                  ]
              ]

    exprNot = MP.char '~' $> Not
    exprIdent = bracketable $ Ident <$> surroundHSpace identifier
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
apply = surroundHSpace $ do
    MP.char '{' *> MP.hspace
    i <- identifier
    MP.space
    a <- sepBy1 arg MP.hspace1
    MP.space <* MP.char '}'
    pure $ Apply i a

arg :: Parser Expr
arg = (Card <$> card) <|> (Num <$> num) <|> bracket expr

num :: (Read a) => Parser a
num = read <$> some MP.digitChar

card :: Parser CardId
card = MP.char '#' *> some MP.digitChar
