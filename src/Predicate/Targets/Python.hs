module Predicate.Targets.Python (
    testTranspile,
    transpile,
) where

import Data.List (intercalate)
import qualified Data.Text as T
import Predicate.Parser

-- >>> testTranspile "main = ({has 3 #17} -> ~{has 1 #2}) | {gt 2 #4} & {lt 3 #1};"
-- "def main(d):\n\treturn (((has(d,3,d[17]) and (not has(d,1,d[2]))) or gt(d,2,d[4])) and lt(d,3,d[1]))"
testTranspile :: T.Text -> String
testTranspile inp =
    case testParser statements inp of
        Left x -> x
        Right s -> transpile "d" "h" s

header :: String
header =
    intercalate
        "\n"
        [ "def lt(hand, n, card):"
        , "\treturn hand.count(card) < n"
        , "def eq(hand, n, card):"
        , "\treturn hand.count(card) == n"
        , "def gt(hand, n, card):"
        , "\treturn hand.count(card) > n"
        , "def has(hand, card):"
        , "\treturn card in hand"
        , "def if_(hand, b, f, g):"
        , "\treturn f if b else g"
        ]
        <> "\n"

transpile :: String -> String -> [Statement] -> String
transpile deckName handName =
    (header <>) . intercalate "\n" . fmap (transpileLine deckName handName)

transpileLine :: String -> String -> Statement -> String
transpileLine deckName handName = \case
    SetDeck xs ->
        let transpiledDeck = intercalate "," ((\(c, n) -> c <> ":" <> show n) <$> xs)
         in concat [deckName, " = ", "{" <> transpiledDeck <> "}"]
    Assign f xs e ->
        concat
            [ "def " <> f
            , "("
            , intercalate "," (handName : xs)
            , "):\n\treturn "
            , transpileExpr handName e
            ]

transpileExpr :: String -> Expr -> String
transpileExpr handName =
    go
  where
    wrap x = "(" : x <> [")"]
    go =
        concat . \case
            Card c -> [c]
            Ident i -> [i]
            Num n -> [show n]
            Not e -> wrap ["not ", go e, ""]
            Or l r -> wrap [go l, " or ", go r]
            And l r -> wrap [go l, " and ", go r]
            Implies l r -> wrap ["(", go (Not l), " or ", go r, ")"]
            Apply f xs ->
                [ transpileExpr handName f
                , "("
                , intercalate "," (handName : (go <$> xs))
                , ")"
                ]
