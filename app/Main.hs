{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import Data.Functor
import Data.List (intercalate)
import qualified Data.Text.IO as T
import Predicate.Parser
import qualified Predicate.Targets.Python as Py
import Text.Megaparsec

main :: IO ()
main = do
    runLang "py"

runLang :: String -> IO ()
runLang lang = do
    let inputName x = concat ["input/", x, ".yug"]
        outputName x = concat ["output/", lang, "/", x, ".", lang]
        readYug x = fmap (x,) . T.readFile . inputName $ x
        writeYug = writeFile . outputName

    tests <- traverse readYug ["test0", "test1"]
    forM_ tests $ \(f, t) -> do
        putStrLn ("\n\n" <> f)
        let deckName = "deck"
            transpile = case lang of
                "py" -> Py.transpile
                _ -> error "Unknown Target"
            parseResult = parse statements f t
            result =
                case fmap (transpile deckName) <$> parseResult of
                    Left e -> errorBundlePretty e
                    Right r -> intercalate "\n" r
        putStrLn result
        writeYug f result
