module Main where

import Options.Applicative
import Utils
import Solver

data Opts = Opts
    {  file :: !String
    }

main :: IO()
main = do
    opts <- execParser optsParser

    
    b <- loadBoard (file opts)
    solve b

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <> progDesc "Hidato haskell implementation" <>
         header
             "Sudoku Hidato")
versionOption :: Parser (a -> a)
versionOption = infoOption "Hidato Version 1.0" (long "version" <> help "Show version")
programOptions :: Parser Opts
programOptions =
    Opts <$> strOption
        (long "file" <> metavar "VALUE" <> value "board" <>
         help "file with generated hidato")
