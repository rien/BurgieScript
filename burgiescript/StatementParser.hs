module StatementParser (parseStatements, parseStat) where

import Definitions
import Parser
import CommandParser
import ExpressionParser

import Data.Char
import Control.Monad (void)
import Control.Applicative ((<|>), many)

-- Parse a list of statementsby using recursion.
-- If the list is not parseable, an empty list is returned.
parseStatements :: Parser [Stat]
parseStatements = try <|> return []
    where try = do
            s  <- parseStat
            xs <- parseStatements  -- recurse!
            return (s:xs)

-- Parses a block: a list of statements between \ and /
parseBlock :: Parser [Stat]
parseBlock = parseParens "\\" parseStatements "/"

parseStat :: Parser Stat
parseStat = chooseFrom [parseComment,
                        parseAssign,
                        parseLoop,
                        parseConditional,
                        parseMBot]

parseComment, parseAssign, parseLoop, parseConditional :: Parser Stat

-- Parse a comment.
-- Comments begin with "Terzijde:" and end with a punctuation mark.
parseComment = do
    match "Terzijde:"
    void $ many $ satisfy $ not . punc
    void $ satisfy punc
    return Comment
  where punc c = c `elem` ['?','!','.']

parseAssign = do
    match "Zet"
    match "variabele"
    stripl            -- Allow spaces before a variable name.
    varName <- many $ satisfy isAlpha
    match "op"
    expr <- parseExp  -- To avoid shadowing the 'exp' function, I have to
    match "."        -- be inconsistent and use 'expr' as variable.
    return $ Assign varName expr

-- Helper method wich parses the condition for a loop or condition.
parseConditionFor :: String -> Parser Exp
parseConditionFor keyword = do
    match keyword
    expr <- parseExp
    match "doe"
    match ":"
    return expr

parseLoop = do
    expr <- parseConditionFor "Gedurende"
    blk <- parseBlock
    return $ Loop expr blk

parseConditional = do
    expr <- parseConditionFor "Indien"
    ifBlk <- parseBlock
    elseBlk <- parseElse <|> return []
    return $ Conditional expr ifBlk elseBlk
  where
    parseElse = do
        match "Anderzijds"
        match ":"
        parseBlock

