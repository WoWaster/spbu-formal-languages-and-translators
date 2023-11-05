module Main where

import Data.Char (isAlpha)
import Data.Foldable.WithIndex
import Data.Map (Map)
import qualified Data.Map as Map
import Options.Applicative
import Text.Pretty.Simple

inputFile :: Parser String
inputFile = argument str (metavar "FILE" <> help "File to read")

type Concordance = Map String [Int]

addLine :: Int -> String -> Concordance -> Concordance
addLine index line state = newConcordance
  where
    textWords = map (takeWhile isAlpha) $ words line
    newConcordance =
        foldr
            ( \word state -> case Map.lookup word state of
                Nothing -> Map.insert word [(index + 1)] state
                Just val -> Map.insert word ((index + 1) : val) state
            )
            state
            textWords

concordance :: String -> IO ()
concordance file = do
    text <- readFile file
    let textLines = lines text
        res = ifoldr addLine Map.empty textLines
    mapM_ (putStrLn . show) (Map.toList res)

main :: IO ()
main = concordance =<< execParser opts
  where
    opts = info (inputFile <**> helper) (fullDesc <> header "Concordance")
