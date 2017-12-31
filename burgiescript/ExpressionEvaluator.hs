module ExpressionEvaluator (runExpEval, evalExp) where

import Definitions

import MBot as M
import qualified Data.HashMap.Strict as Map
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import GHC.Float

-- Lift IO to ExpEval
lift2 :: IO a -> ExpEval a
lift2 = lift . lift

-- The ExpEnv is a tuple (Device, VarMap) which is put in a ReaderT monad
-- because it would not be sane to allow an expression to modify variables.
-- Setting a variable is only allowed with the Assign-statement.
runExpEval :: ExpEnv -> ExpEval a -> IO (Either Error a)
runExpEval env eval = runExceptT $ runReaderT eval env

-- Helper: evaluate expression and check if it matches the expected type
-- if the types do not match, it throws a TypeError
evalAndCheck :: Exp -> Type -> ExpEval Number
evalAndCheck expr exType = do
    (v, t) <- evalExp expr
    when (t /= exType) $
        let msg = "Verwacht type is "   ++ show exType ++
                  " maar was "          ++ show t ++
                  " in de expressie "   ++ show expr
         in throwError (msg, TypeFout)
    return v

-- The main workhorse of this module: evaluate an Exp
evalExp :: Exp -> ExpEval Value
evalExp (Lit n)     = return n                  -- Literal value
evalExp (Var name)  = do                        -- Variable lookup
    (_, vm) <- ask
    let v  = Map.lookup name vm
     in case v of
        Nothing  -> let msg = "Veranderlijke " ++ name ++
                              "werd (nog) niet gedefiniëerd."
                     in throwError (msg, OngebondenVeranderlijke)
        Just val -> return val
evalExp (UnExp (UnFun at rt f) expr) = do       -- Unary operator
    v <- evalAndCheck expr at
    return (f v, rt)
evalExp (BiExp (BiFun argt rett f) a b) = do    -- Binary operator
    av <- evalAndCheck a argt
    bv <- evalAndCheck b argt
    return (f av bv, rett)
evalExp (Query Sonar) = do                      -- Sonar Query
    (d,_) <- ask
    f <- lift2 $ M.readUltraSonic d
    return (float2Double f, NumT)
evalExp (Query (Line side)) = do                -- Line Query
    (d,_) <- ask
    l <- lift2 $ M.readLineFollower d
    case side of
        Portside  -> return $ isWhiteLeft l
        Starboard -> return $ isWhiteRight l
  where
    yes   = (1, BoolT)
    no    = (0, BoolT)
    isWhiteLeft l = case l of
                        M.BOTHW  -> yes
                        M.RIGHTB -> yes
                        _        -> no
    isWhiteRight l = case l of
                        M.BOTHW  -> yes
                        M.LEFTB  -> yes
                        _        -> no
