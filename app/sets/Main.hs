module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Options.Applicative

data Files = Files String String

printUnion :: Set Char -> Set Char -> IO ()
printUnion set1 set2 = do
    putStr "Union of sets: "
    print $ Set.toList $ Set.union set1 set2

printDiff :: Set Char -> Set Char -> IO ()
printDiff set1 set2 = do
    putStr "Difference of sets: "
    print $ Set.toList $ Set.difference set1 set2

printIntersect :: Set Char -> Set Char -> IO ()
printIntersect set1 set2 = do
    putStr "Intersection of sets: "
    print $ Set.toList $ Set.intersection set1 set2

sets :: Files -> IO ()
sets (Files file1 file2) = do
    text1 <- readFile file1
    text2 <- readFile file2
    let symbols1 = Set.fromList text1
        symbols2 = Set.fromList text2
     in do
            printUnion symbols1 symbols2
            printIntersect symbols1 symbols2
            printDiff symbols1 symbols2

pFiles :: Parser Files
pFiles =
    Files
        <$> argument str (metavar "FILE1" <> help "First file with text")
        <*> argument str (metavar "FILE2" <> help "Second file with text")

main :: IO ()
main = sets =<< execParser (info (pFiles <**> helper) (fullDesc <> header "Sets"))