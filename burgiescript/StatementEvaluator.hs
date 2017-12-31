module StatementEvaluator (evalStatements, runStatEval, evalStat) where

import Definitions
import ExpressionEvaluator
import CommandEvaluator

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar (readMVar)
import qualified Data.HashMap.Strict as Map

-- Run the statements.
-- * A StatEnv with the device handle and an MVar containing a boolean
--   which indicates if the program needs to be interrupted is put in a
--   Reader monad, because these are read-only.
-- * The HashMap containing all the variables is put in the State monad,
--   because this can change with every Assign statement.
-- * The state and result (mostly the Unit) is wrapped in the Either monad
--   because Error handling.
runStatEval :: StatEnv -> VarMap -> StatEval a -> IO (Either Error a, VarMap)
runStatEval env vm eval = runStateT ( runExceptT $ runReaderT eval env ) vm

-- Powerlift
lift3 :: IO a -> StatEval a
lift3 = lift . lift . lift

-- Because typing 'return ()' is 6 charachters too much.
nop :: StatEval ()
nop = return ()

-- Calculate the value of an expression, without worrying about
-- environments, lifting or errors.
calculate :: Exp -> StatEval Value
calculate expr = do
    vm  <- get
    (d,_) <- ask
    result <- lift3 $ runExpEval (d,vm) $ evalExp expr
    either throwError return result

-- Execute a list (block) of statements.
-- Because after each statement evaluation this function
-- is executed, there is a check here to see wheter the
-- program's termination is requested.
evalStatements :: [Stat] -> StatEval ()
evalStatements [] = nop
evalStatements (st:sts) = do
    (_,mv) <- ask       -- Termination check
    isInterrupt <- lift3 $ readMVar mv
    when isInterrupt $ throwError ("Evaluatie gestopt", GebruikersOnderbreking)
    evalStat st         -- Handle the next statement and loop
    evalStatements sts

-- Decide, based on the given expression, whether to execute the first or the
-- second monadic action.
-- When the type of the expression is not a BoolT, an error is thrown.
decide :: Exp -> StatEval () -> StatEval () -> StatEval ()
decide expr yes no = do
    (v, t) <- calculate expr
    case t of
        BoolT -> case v of
                    1 -> yes
                    0 -> no
                    _ -> error "BoolT with wrong value"
        _     -> throwError ("Type van de voorwaarde was: " ++ show t,
                             VerbodenVoorwaardeType)

-- Evaluate one statement
evalStat :: Stat -> StatEval ()
evalStat Comment = nop                              -- Comment
evalStat (Assign name expr) = do                     -- Assignment
    val <- calculate expr
    modify $ Map.insert name val
evalStat (MBot cmd) = do                            -- MBot command
    (dev,_) <- ask
    lift3 $ runCmdEval dev $ evalCommand cmd
-- Loop
evalStat l@(Loop expr blk) = decide expr (evalStatements blk >> evalStat l) nop
-- Conditional
evalStat (Conditional expr ifBlk elseBlk) = decide expr (evalStatements ifBlk)
                                                        (evalStatements elseBlk)




