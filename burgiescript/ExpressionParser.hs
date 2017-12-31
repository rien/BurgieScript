module ExpressionParser (parseExp) where

import Parser
import Definitions
import CommandParser (parseSide)

import Control.Applicative ((<|>), some)
import Data.Char (isDigit, isAlpha)

-- Notes:
-- * No distinction is made between a numerical of a boolean expression
--   while parsing. I have chosen to throw an error while running the program
--   when an expression is used within the wrong type context.
-- * Every expression has four 'levels': an expression, a comparable expression,
--   a term and a factor. On each level there are binary operators which produce
--   a result on a level higher. This makes it possible for multiplication to
--   have a higher precedence than addition for example.

parseLit, parseVar, parseQuery, parseTerm, parseFact, parseComp, parseExp :: Parser Exp

-- Numerical Binary Function
nbf :: (Number -> Number -> Number) -> BiFun
nbf = BiFun NumT NumT

-- Boolean Binary Function
bbf :: (Number -> Number -> Number) -> BiFun
bbf = BiFun BoolT BoolT

-- The or, and and not binary functions but with numbers
numOr, numAnd :: Number -> Number -> Number
numOr  1 _ = 1
numOr  _ x = x
numAnd 0 _ = 0
numAnd _ x = x

numNot :: Number -> Number
numNot 0 = 1
numNot _ = 0

-- Parse an expression
parseExp = chain parseComp $ parseStrTo [ ("overeenkomstig met"     , cbf (==))
                                        , ("verschillend met"       , cbf (/=))
                                        , ("significanter dan"      , cbf (>))
                                        , ("minder significant dan" , cbf (<))
                                        ]
  where  -- cbf: Comparative Binary Function
    cbf f = BiFun NumT BoolT $ toNum f
    toNum f a b = if f a b
                    then 1
                    else 0

-- Parse a comparable expression
parseComp = chain parseTerm $ parseStrTo
    [ ("+"      , nbf (+))
    , ("-"      , nbf (-))
    , ("hetzij" , bbf numOr)
    ]

-- Parse a term
parseTerm = chain parseFact $ parseStrTo
    [ ("x"      , nbf (*))
    , (":"      , nbf (/))
    , ("tevens" , bbf numAnd)
    ]

-- Parse a factor
parseFact = chooseFrom
    [ parseNegation base "-" numNegate
    , parseNegation base "allesbehalve" boolNegate
    , base
    ]
  where
    numNegate         = UnFun NumT NumT negate
    boolNegate        = UnFun BoolT BoolT numNot
    base = chooseFrom
        [ parseLit
        , parseParens "(" parseExp")"
        , parseQuery
        , parseVar
        ]


-- Literal number or boolean
-- A number can have a fractional part after a comma
parseLit = parseNumLit <|> parseStrTo
    [ ("waarachtig", Lit (1, BoolT))
    , ("strijdig",   Lit (0, BoolT))
    ]
  where
    takeDigits = some $ satisfy isDigit
    parseNumLit = do
        stripl
        s1 <- takeDigits
        s2 <- afterComma <|> return []
        return $ Lit (read (s1 ++ s2), NumT)
    afterComma = do
        match ","
        s <- takeDigits
        return ('.':s)

-- Parse a variable name
parseVar = do
    stripl
    name <- some $ satisfy isAlpha
    return $ Var name

-- Query: request a measurement from one of the MBot's sensors
parseQuery = sonar <|> line
  where
    sonar = do
        match "GeluidWeerkaatsingsApparaatWaarde"
        return $ Query Sonar
    line = do
        match "LijnVolgApparaatIsWit"
        side <- parseSide
        return $ Query $ Line side

-- Negation: an expression preceded by an unary operator
parseNegation :: Parser Exp -> String -> UnFun -> Parser Exp
parseNegation without token unfun = do
    match token
    e <- without
    return $ UnExp unfun e
