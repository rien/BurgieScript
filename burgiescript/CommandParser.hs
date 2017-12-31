module CommandParser (parseMBot, parseSide) where

import Parser
import Definitions

import Control.Applicative

-- Note:
-- This module really does not need much explenation. Each command
-- is parsed by looking for an exact string.

parseMotorCommand, parseSoundCommand, parseLightCommand, parseSleep :: Parser Command

-- Combine all the individual command parsers
parseMBot :: Parser Stat
parseMBot = MBot <$> chooseFrom
    [ parseMotorCommand
    , parseSoundCommand
    , parseLightCommand
    , parseSleep
    ]

parseSide :: Parser Side
parseSide = do
    match "te"
    parseStrTo
        [ ("bakboordzijde"   , Portside)
        , ("stuurboordzijde" , Starboard)
        ]

parseDirection :: Parser Direction
parseDirection = chooseFrom [pLineal, pLateral]
  where
    pLateral  = Lat <$> parseSide
    pLineal = parseStrTo
        [ ("voorwaarts", Forward)
        , ("rugwaarts",  Backward)
        ]


parseMotorCommand = chauffeer <|> stop
  where
    chauffeer = do
        match "Chauffeer"
        dir <- parseDirection
        match "!"
        return $ Motor dir
    stop = do
        match "Halt!"
        return $ Motor Stop


parseSoundCommand = do
    match "Zing"
    duration <- parseStrTo
        [ ("kortstondig", 125)
        , ("eventjes",    250)
        , ("langdurig",   500)
        , ("erg langdurig", 2000)
        ]
    match "een"
    frequency <- parseStrTo
        [ ("do",  523)
        , ("re",  587)
        , ("mi",  659)
        , ("fa",  698)
        , ("sol", 784)
        , ("la",  880)
        , ("si",  988)
        ]
    match "!"
    return $ Sound (frequency, duration)


parseLightCommand = parseLightsOff <|> parseLightsOn
  where
    parseLightsOff = do
        match "Verduister"
        side <- parseSide
        match "!"
        return $ Light side (0,0,0)
    parseLightsOn = do
        match "Kleur"
        color <- parseStrTo
            [ ("rood"   , (100  , 0     , 0     ))
            , ("groen"  , (0    , 100   , 0     ))
            , ("blauw"  , (0    , 0     , 100   ))
            , ("wit"    , (100  , 100   , 100   ))
            ]
        side <- parseSide
        match "!"
        return $ Light side color

parseSleep = do
    time <- parseStrTo
        [ ("Rust kort!"                     , Short)
        , ("Neem op je gemak een pauze!"    , Normal)
        , ("Sluit je ogen maar voor even!"  , Long)
        , ("Droom zacht zoete prins!"       , AlmostEternally)
        ]
    return $ Sleep time

