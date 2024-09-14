{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
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
        let dashes = replicate 20 '-'
        putStrLn $ dashes <> f <> dashes

        let deckName = "deck"
            transpile = case lang of
                "py" -> Py.transpile
                _ -> error "Unknown Target"
            parseResult = parse statements f t
            transpileResult = fmap (transpile deckName) <$> parseResult

        case transpileResult of
            Left e -> putStrLn $ errorBundlePretty e <> "\n"
            Right r -> writeYug f $ intercalate "\n" r
