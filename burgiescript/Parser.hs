module Parser where

import Definitions

import Control.Monad
import Control.Applicative
import Data.Char

-- Definition here to avoid it becoming an orphan instance
newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
    p >>= k  = Parser $ \s ->
                [ (x2, s2) |
                  (x1, s1) <- apply p s,
                  (x2, s2) <- apply (k x1) s1]

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (<*>)  = ap

instance MonadPlus Parser where
    mzero = Parser $ const []
    mplus m1 m2 = Parser $ \s -> apply m1 s ++ apply m2 s

-- Enables some (one or more) and many (zero or more)
instance Alternative Parser where
    -- When there are multiple parsing options, select the first one
    pa <|> pb = Parser $ \s -> case apply (mplus pa pb) s of
                                []    -> []
                                (x:_) -> [x]
    empty = mzero

-- Parser application
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

-- Try to parse a string (trailing newlines are ignored) with a given Parser
parse :: Parser a -> String -> Either Error a
parse p str = let parsed        = apply (stript p) str
                  complete      = filter (null . snd) parsed -- Completely parsed
                  failWith msg  = Left (msg, OntleedFout)
              in case complete of
                ((x,_):_)     -> Right x
                []  -> case parsed of
                        ((_,s):_) -> failWith $ "Ontleding niet gelukt:\n"
                                                ++ show s
                        []        -> failWith "Onbekende fout"

-- Choose from a list of parser by selecting the first successful one
chooseFrom :: [Parser a] -> Parser a
chooseFrom = foldl1 (<|>)

-- Take one character
char :: Parser Char
char = Parser f
  where
    f []     = []
    f (c:cs) = [(c,cs)]

-- Take a character if it matches a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = do
    c <- char
    guard $ pr c
    return c


-- Match a given character
takeChar :: Char -> Parser Char
takeChar = satisfy . (==)

-- Match a given string and remove trailing whitespace. Returns nothing.
match :: String -> Parser ()
match str = stripl >> mapM_ takeChar str

-- Throw away leading whitespace
stripl :: Parser ()
stripl = void $ many $ satisfy isSpace

-- Throw away trailing whitespace
stript :: Parser a -> Parser a
stript p = do
    result <- p
    stripl
    return result


-- Easy conversion of exact strings to custom values
parseStrTo :: [(String, a)] -> Parser a
parseStrTo list = chooseFrom [ match str >> return a
                             | (str, a) <- list ]

-- Parse something between a pair of strings
parseParens :: String -> Parser a -> String -> Parser a
parseParens open parser close = do
    match open
    p <- parser
    match close
    return p

-- Try to parse an operation between two expressions (ex. a + b),
-- if it fails we return the first expression ( a ).
chain :: Parser Exp -> Parser BiFun -> Parser Exp
chain exParser opParser = do
    a <- exParser
    rest a <|> return a
  where
    rest a = do
        bif <- opParser
        b  <- chain exParser opParser
        return $ BiExp bif a b

