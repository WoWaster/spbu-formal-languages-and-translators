module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Options.Applicative
import Text.Pretty.Simple

data Files = Files String String

printUnion :: Set Char -> Set Char -> IO ()
printUnion set1 set2 = do
    putStr "Union of sets: "
    pPrint $ Set.toList $ Set.union set1 set2

printDiff :: Set Char -> Set Char -> IO ()
printDiff set1 set2 = do
    putStr "Difference of sets: "
    pPrint $ Set.toList $ Set.difference set1 set2

printIntersect :: Set Char -> Set Char -> IO ()
printIntersect set1 set2 = do
    putStr "Intersection of sets: "
    pPrint $ Set.toList $ Set.intersection set1 set2

printSize :: Int -> Set Char -> IO ()
printSize i set = do
    putStr $ "Size of alphabet " ++ show i ++ " "
    print $ Set.size set

printSizeOfIntersection :: Set Char -> Set Char -> IO ()
printSizeOfIntersection set1 set2 = do
    putStr "Size of set1 \\ set2 "
    print $ Set.size $ Set.difference set1 set2

sets :: Files -> IO ()
sets (Files file1 file2) = do
    text1 <- readFile file1
    text2 <- readFile file2
    let symbols1 = Set.fromList text1
        symbols2 = Set.fromList text2
    printUnion symbols1 symbols2
    printIntersect symbols1 symbols2
    printDiff symbols1 symbols2
    printSize 1 symbols1
    printSize 2 symbols2
    printSizeOfIntersection symbols1 symbols2

pFiles :: Parser Files
pFiles =
    Files
        <$> argument str (metavar "FILE1" <> help "First file with text")
        <*> argument str (metavar "FILE2" <> help "Second file with text")

main :: IO ()
main = sets =<< execParser (info (pFiles <**> helper) (fullDesc <> header "Sets"))
