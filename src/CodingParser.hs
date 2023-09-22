{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodingParser where

import Data.Char (isAlphaNum, isPrint)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

data Rule = Rule {nonterminal :: Text, productions :: [Production]}
    deriving (Eq, Show)

pIdentifier :: Maybe String -> Parser Text
pIdentifier msg = lexeme (takeWhile1P msg isAlphaNum)

data Production
    = ProdParentheses [Production]
    | ProdAsterisk
    | ProdSemicolon
    | ProdComma
    | ProdHash
    | ProdBrackets [Production]
    | ProdTerminal Text
    | ProdSemantic Text
    | ProdTerminalNonterminal Text
    deriving (Eq, Show)

isPrintNotBraces c = isPrint c && c /= '\''

pTerminal :: Parser Text
pTerminal = lexeme $ between (symbol "'") (symbol "'") (takeWhile1P Nothing isPrintNotBraces)

pSemantic :: Parser Text
pSemantic = lexeme $ symbol "$" *> pIdentifier (Just "semantic")

pNonterminal :: Parser Text
pNonterminal = pIdentifier (Just "nonterminal")

pProduction :: Parser Production
pProduction =
    choice
        [ ProdParentheses <$> between (symbol "(") (symbol ")") (many pProduction)
        , ProdBrackets <$> between (symbol "[") (symbol "]") (many pProduction)
        , ProdAsterisk <$ symbol "*"
        , ProdSemicolon <$ symbol ";"
        , ProdComma <$ symbol ","
        , ProdHash <$ symbol "#"
        , ProdTerminalNonterminal <$> pIdentifier (Just "terminal or nonterminal")
        , ProdTerminal <$> pTerminal
        , ProdSemantic <$> pSemantic
        ]

pProductions :: Parser [Production]
pProductions = many pProduction

pRule :: Parser Rule
pRule = do
    nonterminal <- pNonterminal
    _ <- symbol ":"
    productions <- pProductions
    _ <- symbol "."
    return Rule{..}

pRules :: Parser [Rule]
pRules = do
    rules <- some $ try pRule
    _ <- lexeme (string "Eofgram")
    _ <- eof
    return rules
