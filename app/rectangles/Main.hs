module Main where

import Options.Applicative

data Dimensions = Dimensions {width :: Int, height :: Int} deriving (Show, Eq)
opts :: Parser Dimensions
opts =
    Dimensions
        <$> argument auto (metavar "WIDTH" <> help "Width of a rectangle")
        <*> argument auto (metavar "HEIGHT" <> help "Height of a rectangle")

rectanglesHelper :: Dimensions -> IO ()
rectanglesHelper dims =
    let
        width' = width dims
        height' = height dims
        res = sum $ [(width' - i + 1) * (height' - j + 1) | i <- [1 .. width'], j <- [1 .. height']]
     in
        do print res

main :: IO ()
main = rectanglesHelper =<< execParser (info (opts <**> helper) (fullDesc <> header "Rectangles"))
