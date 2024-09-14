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

func1 = {has 3 #1} | {has 1 #2};

func2 = ~ {has 3 #3};
 -}

module Predicate.Parser (

) where

import Control.Monad
import Data.Function
import Data.Functor
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Parser.Combinators

{- Parser -}
type Parser = Parsec Void T.Text

testParser :: Parser a -> T.Text -> Either String a
testParser p test = do
    parse p "source-file" test & \case
        Left bundle -> Left $ errorBundlePretty bundle
        Right s -> Right s

{- Data Types -}
type Identifier = String

data Stmt = Assign Identifier Expr
    deriving (Show, Eq)

data Expr
    = Not Expr
    | Or Expr Expr
    | And Expr Expr
    | Implies Expr Expr
    | Apply Identifier [Expr]
    | Ident Identifier
    | Card String
    deriving (Show, Eq)

data Dir2D = Backwards | Forwards
    deriving (Show, Eq)

function :: Parser (String, Expr)
function = do
    n <- identifier
    void $ surround space $ char '='
    e <- expr
    pure (n, e)

-- >>> testParser identifier "function"
-- >>> testParser identifier "!function"
-- Right "function"
-- Left "source-file:1:1:\n  |\n1 | !function\n  | ^\nunexpected '!'\nexpecting letter\n"
identifier :: Parser Identifier
identifier = letterChar <:> many alphaNumChar

maxTerm :: Int
maxTerm = 2

-- >>> testParser expr "e1"
-- >>> testParser expr "e1 | e2 & e3"
-- >>> testParser expr "e1 & e2 | e3"
-- >>> testParser expr "e1 & ( e2 | e3 )"
-- >>> testParser expr "e1 -> e2 & e3"
-- >>> testParser expr "e1 | e2 -> e3 & e4"
-- >>> testParser expr "(e1 | e2) -> e3 & e4"
-- >>> testParser expr "e1 -> e2 -> e3"
-- Right (Ident "e1")
-- Right (And (Or (Ident "e1") (Ident "e2")) (Ident "e3"))
-- Right (Or (And (Ident "e1") (Ident "e2")) (Ident "e3"))
-- Right (And (Ident "e1") (Or (Ident "e2") (Ident "e3")))
-- Right (Implies (Ident "e1") (And (Ident "e2") (Ident "e3")))
-- Right (Implies (Or (Ident "e1") (Ident "e2")) (And (Ident "e3") (Ident "e4")))
-- Right (Implies (Or (Ident "e1") (Ident "e2")) (And (Ident "e3") (Ident "e4")))
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
        (unOps >>= (<$> term))
            <|> surround
                space
                ( choice
                    [ bracket expr
                    , bracketable exprIdent
                    , bracketable apply
                    ]
                )

    unOps = choice [exprNot]
    binOps n =
        concat . take n $
            [ [(1,Forwards,) <$> exprImpl]
            ,
                [ (2,Backwards,) <$> exprOr
                , (2,Backwards,) <$> exprAnd
                ]
            ]

    exprNot = char '~' $> Not
    exprIdent = bracketable $ Ident <$> surround space identifier
    exprOr = char '|' $> Or
    exprAnd = char '&' $> And
    exprImpl = string "->" $> Implies

bracket :: Parser a -> Parser a
bracket p = char '(' *> p <* char ')'

bracketable :: Parser a -> Parser a
bracketable p = bracket p <|> p

-- >>> testParser apply "{a #2 #1 2}"
-- >>> testParser apply "{a ({has 2} | {give 3})}"
-- >>> testParser apply "{a}"
-- Right (Apply "a" [Card "2",Card "1",Ident "2"])
-- Right (Apply "a" [Or (Apply "has" [Ident "2"]) (Apply "give" [Ident "3"])])
-- Left "source-file:1:3:\n  |\n1 | {a}\n  |   ^\nunexpected '}'\nexpecting '#', '(', alphanumeric character, digit, or white space\n"
apply :: Parser Expr
apply = surround space $ do
    void $ char '{'
    space
    i <- identifier
    space
    a <- sepBy1 arg space1
    space
    void $ char '}'
    pure $ Apply i a

arg :: Parser Expr
arg = card <|> (Ident <$> some digitChar) <|> bracket expr
  where
    card = Card <$> (char '#' *> some digitChar)
