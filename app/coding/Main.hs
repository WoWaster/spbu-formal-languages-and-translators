{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coding
import CodingParser (pRules)
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO
import Options.Applicative
import Text.Megaparsec hiding (State)
import Prelude hiding (putStrLn, readFile)

inputFile :: Parser String
inputFile = argument str (metavar "FILE" <> value "expression.txt" <> help "File to read")

coding :: String -> IO ()
coding file = do
    text <- readFile file
    let rules = runParser pRules file text
    case rules of
        Left bundle -> putStrLn $ T.pack (errorBundlePretty bundle)
        Right result -> putStrLn $ codingWrapper result

main :: IO ()
main = coding =<< execParser opts
  where
    opts = info (inputFile <**> helper) (fullDesc <> header "Coding")
