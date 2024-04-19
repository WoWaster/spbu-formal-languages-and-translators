import Data.List
import Debug.Trace

applyRule :: String -> (String, String) -> String
applyRule sentence rule@(nonTerm, sentenceToReplace) =
    case sentence of
        [] -> []
        (l : ls) -> if [l] == nonTerm then sentenceToReplace ++ ls else l : applyRule ls rule

data Grammar = Grammar
    { terminals :: [String]
    , nonTerminals :: [String]
    , products :: [(String, String)]
    , start :: String
    }

leftDerivation :: [Int] -> Grammar -> String
leftDerivation rulesNumbers (Grammar _ _ products start) = seqOfDerive start rulesNumbers
  where
    seqOfDerive res rNum = case rNum of
        [] -> res
        (number : ns) -> seqOfDerive (applyRule res (products !! number)) ns

directSum :: (Eq a) => Int -> [[a]] -> [[a]] -> [[a]]
directSum k l1 l2
    | null l1 = []
    | null l2 = []
    | otherwise =
        nub
            [ take k (x ++ y)
            | x <- l1
            , y <- l2
            ]

first :: Grammar -> Int -> String -> [String]
first (Grammar terminals nonTerminals products start) k = f 4 -- ((length terminals + 1) ^ k) -- upper bound
  where
    f i var
        | var `elem` terminals = trace ("term f " ++ show i ++ " " ++ show var) [var]
        | i == 0 =
            trace
                (" i = 0 f " ++ (show i) ++ " " ++ (show var))
                nub
                [ take k $ takeWhile (\x -> [x] `elem` terminals) derivation
                | (nt, derivation) <- products
                , nt == var
                ]
        | otherwise =
            trace
                ("else f " ++ (show i) ++ " " ++ (show var))
                union
                ( f
                    (i - 1)
                    var
                )
                ( foldl
                    union
                    []
                    [ let x =
                            foldl1
                                (directSum k)
                                $ map
                                    (\x -> if x == [""] then [] else x)
                                    ( map
                                        (\char -> f (i - 1) [char])
                                        derivation
                                    )
                       in trace ("inner " ++ show x) $ x
                    | (nt, derivation) <- products
                    , nt == var
                    ]
                )

main :: IO ()
main = do
    let testGrammar =
            Grammar
                { terminals = ["a", "+", "*", "(", ")"]
                , nonTerminals = ["E", "G", "T", "Y", "F"]
                , products =
                    [ ("E", "TG")
                    , ("G", "+TG")
                    , ("G", "")
                    , ("T", "FY")
                    , ("Y", "*FY")
                    , ("Y", "")
                    , ("F", "(E)")
                    , ("F", "a")
                    ]
                , start = "E"
                }
    mapM_ (print . first testGrammar 1) $ nonTerminals testGrammar
