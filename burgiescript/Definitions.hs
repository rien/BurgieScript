module Definitions where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar (MVar)

import System.HIDAPI (Device)
import qualified Data.HashMap.Strict as Map


type Number  = Double           -- Our number type
type Note    = (Int, Int)       -- (frequency, duration)
type Color   = (Int, Int, Int)  -- (Red, Green, Blue)
type VarName = String           -- Sadly there is no type which enforces

type Value = (Number, Type)     -- Everything is a number,
data Type = NumT | BoolT        -- but we still want to typecheck
            deriving (Show, Eq)

type Error = (String, ErrorType)
data ErrorType = TypeFout       -- Errors, in Dutch ofcourse!
                | OngebondenVeranderlijke
                | VerbodenVoorwaardeType
                | OntleedFout
                | GebruikersOnderbreking
                deriving Show

data Exp = Lit Value
           | Var VarName
           | UnExp UnFun Exp
           | BiExp BiFun Exp Exp
           | Query Sensor
           deriving Show

--                 Args Ret  Function itself
data UnFun = UnFun Type Type (Number -> Number)
data BiFun = BiFun Type Type (Number -> Number -> Number)

-- Because functions do not implement Show
instance Show UnFun where
    show (UnFun at rt _) = show at ++ "->" ++ show rt
instance Show BiFun where
    show (BiFun at rt _) = show at ++ "->" ++ show rt

data Sensor = Sonar | Line Side deriving Show

data Stat = Comment
            | Assign VarName Exp
            | MBot Command
            | Loop Exp [Stat]
            | Conditional Exp [Stat] [Stat]
            deriving Show

data Command = Motor Direction
             | Sound Note
             | Light Side Color
             | Sleep Time
             deriving Show

data Time = Short | Normal | Long | AlmostEternally deriving Show

data Direction = Forward
               | Backward
               | Stop
               | Lat Side
               deriving Show

data Side = Portside | Starboard deriving Show


type VarMap = Map.HashMap VarName Value     -- Where variables are stored

type ExpEnv  = (Device, VarMap)             -- See: ExpressionEvaluator
type StatEnv = (Device, MVar Bool)          -- See: StatementEvaluator

-- << Insert Tranformers joke here >>
type CmdEval  a = ReaderT Device IO a
type ExpEval  a = ReaderT ExpEnv ( ExceptT Error IO ) a
type StatEval a = ReaderT StatEnv ( ExceptT Error ( StateT VarMap IO )) a
