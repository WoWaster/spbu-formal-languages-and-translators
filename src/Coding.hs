{-# LANGUAGE OverloadedStrings #-}

module Coding where

import CodingParser
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

data CodingState = CodingState
    { nonterminalCount :: Int
    , nonterminalBound :: Int
    , terminalCount :: Int
    , terminalBound :: Int
    , semanticCount :: Int
    , semanticBound :: Int
    , nonterminals :: [Text]
    , nonterminalMap :: HashMap.HashMap Text Int
    , terminalMap :: HashMap.HashMap Text Int
    , semanticMap :: HashMap.HashMap Text Int
    }

encodeProduction :: Production -> State CodingState Builder
encodeProduction (ProdParentheses productions) = do
    innerProductionsWithState <- mapM (encodeProduction) productions
    let innerProductions = foldr (<>) "" innerProductionsWithState
    return $ fromLazyText "2 " <> innerProductions <> fromLazyText "3 "
encodeProduction ProdAsterisk = return $ fromLazyText "5 "
encodeProduction ProdSemicolon = return $ fromLazyText "6 "
encodeProduction ProdComma = return $ fromLazyText "7 "
encodeProduction ProdHash = return $ fromLazyText "8 "
encodeProduction (ProdBrackets productions) = do
    innerProductionsWithState <- mapM (encodeProduction) productions
    let innerProductions = foldr (<>) "" innerProductionsWithState
    return $ fromLazyText "9 " <> innerProductions <> fromLazyText "10 "
encodeProduction (ProdTerminal text) = do
    state <- get

    case HashMap.lookup text (terminalMap state) of
        Just num -> return $ fromString (show num ++ " ")
        Nothing -> do
            let num = terminalCount state
            put $ state{terminalCount = terminalCount state + 1, terminalMap = HashMap.insert text num (terminalMap state)}
            return $ fromString (show num ++ " ")
encodeProduction (ProdSemantic text) = do
    state <- get

    case HashMap.lookup text (semanticMap state) of
        Just num -> return $ fromString (show num ++ " ")
        Nothing -> do
            let num = semanticCount state
            put $ state{semanticCount = semanticCount state + 1, semanticMap = HashMap.insert text num (semanticMap state)}
            return $ fromString (show num ++ " ")
encodeProduction (ProdTerminalNonterminal text) = do
    state <- get

    if text `elem` nonterminals state
        then case HashMap.lookup text (nonterminalMap state) of
            Just num -> return $ fromString (show num ++ " ")
            Nothing -> do
                let num = nonterminalCount state
                put $
                    state{nonterminalCount = nonterminalCount state + 1, nonterminalMap = HashMap.insert text num (nonterminalMap state)}
                return $ fromString (show num ++ " ")
        else case HashMap.lookup text (terminalMap state) of
            Just num -> return $ fromString (show num ++ " ")
            Nothing -> do
                let num = terminalCount state
                put $ state{terminalCount = terminalCount state + 1, terminalMap = HashMap.insert text num (terminalMap state)}
                return $ fromString (show num ++ " ")

encodeRule :: Rule -> State CodingState Builder
encodeRule rule = do
    state <- get
    productionsWithState <- mapM (encodeProduction) (productions rule)
    let productions = foldr (<>) "" productionsWithState
    nonterminalNum <- case HashMap.lookup (nonterminal rule) (nonterminalMap state) of
        Just num -> return $ fromString (show num ++ " ")
        Nothing -> do
            let num = nonterminalCount state
            put $
                state
                    { nonterminalCount = nonterminalCount state + 1
                    , nonterminalMap = HashMap.insert (nonterminal rule) num (nonterminalMap state)
                    }
            return $ fromString (show num ++ " ")

    return $
        nonterminalNum
            <> fromLazyText "1 "
            <> productions
            <> fromLazyText "4\n"

encodeRules :: [Rule] -> State CodingState Text
encodeRules rules = do
    rulesWithState <- mapM (encodeRule) rules
    let rules' = foldr (<>) "" rulesWithState
    return $ toLazyText $ rules' <> fromLazyText "1000\n"

codingWrapper :: [Rule] -> Text
codingWrapper rules =
    evalState
        (encodeRules rules)
        CodingState
            { nonterminalCount = 11
            , nonterminalBound = 51
            , terminalCount = 51
            , terminalBound = 101
            , semanticCount = 101
            , semanticBound = 151
            , nonterminals = map nonterminal rules
            , nonterminalMap = HashMap.fromList []
            , terminalMap = HashMap.fromList []
            , semanticMap = HashMap.fromList []
            }
