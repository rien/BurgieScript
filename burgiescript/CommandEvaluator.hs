module CommandEvaluator where

import MBot
import Definitions

import System.HIDAPI (Device)
import Control.Monad.Reader
import Control.Concurrent

-- Run the command evaluator.
runCmdEval :: Device -> CmdEval a -> IO a
runCmdEval dev eval = runReaderT eval dev

-- Evaluate a command
evalCommand :: Definitions.Command -> CmdEval ()
evalCommand (Motor dir) = do                            -- Motor
    d <- ask
    lift $ case dir of
        Stop            -> stop d
        Forward         -> goAhead d
        Backward        -> goBackwards d
        Lat Portside    -> goLeft d
        Lat Starboard   -> goRight d
evalCommand (Sound note) = do                           -- Sound
    d <- ask
    lift $ sendCommand d $ uncurry playTone note
evalCommand (Light side (r,g,b)) = do                   -- Light
    d <- ask
    let i = case side of
                Portside    -> 1
                Starboard   -> 2
     in lift $ sendCommand d $ setRGB i r g b
evalCommand (Sleep time) = lift $ threadDelay delay     -- Sleep
  where
    delay = case time of
                Short           -> 250000
                Normal          -> 1000000
                Long            -> 2000000
                AlmostEternally -> 10000000

