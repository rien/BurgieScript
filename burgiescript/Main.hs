{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- ^ execExp wordt nergens gebruikt in  de code en zal een warning geven
-- Het is echter een handige functie om te debuggen. Omdat ik -Werror en -Wall
-- gebruik om te compileren is bovenstaande optie dus nodig.
import MBot
import Definitions
import Parser
import ExpressionParser
import ExpressionEvaluator
import StatementParser
import StatementEvaluator

import Control.Monad (void, when)
import Control.Concurrent.MVar (newMVar, swapMVar)
import System.Environment (getArgs)
import System.Exit (die)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import qualified Data.HashMap.Strict as Map

main :: IO ()
main =  do
    args <- getArgs
    when (length args /= 1)
        $ die "Één (1) argument nodig: het pad naar het uit te voeren bestand."
    executeFile $ head args

-- Parse and evaluate file contents.
executeFile :: FilePath -> IO ()
executeFile file = do
    program <- parseFile file
    -- If parsing failed: pass the error,
    -- If parsing succeeded: execute the statements
    result  <- either (return . Left) executeStatements program
    either failure success result
    putStrLn "Vaarwel."
    where failure (msg, err) = do
            putStrLn "!! De volgende fout werd opgegooid:"
            putStrLn $ show err ++ ": " ++ msg
          success _ = putStrLn "Het programma werd zonder fouten uitgevoerd."


-- Parse a file.
parseFile :: FilePath -> IO (Either Error [Stat])
parseFile file = do
    cont <- readFile file
    return $ parse parseStatements cont

-- Evaluate a list of statements
executeStatements :: [Stat] -> IO (Either Error ())
executeStatements program = do
    -- Install handler to catch Control-C
    intMVar <- newMVar False
    let handleInt = void $ swapMVar intMVar True
     in do
        void $ installHandler sigINT  (Catch handleInt) Nothing
        void $ installHandler sigTERM (Catch handleInt) Nothing

    d <- openMBot
    putStrLn "Opgepast, we gaan starten!"
    -- Run the parsed statements
    (result, vm) <- runStatEval (d, intMVar) Map.empty $ evalStatements program
    -- Stop the MBot before closing it to avoid accidents
    stop d
    closeMBot d
    putStrLn "Programma afgelopen."
    print vm
    return result

-- Execute a string with an expression
execExp :: String -> IO (Either Error Value)
execExp str = either (return . Left) exec $ parse parseExp str
  where
    exec e = runExpEval undefined $ evalExp e


