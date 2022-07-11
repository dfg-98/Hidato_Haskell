module Main where

import Options.Applicative
import Generator
import Utils

data Opts = Opts
    {
    width :: !Int,
    height :: !Int,  
    minvalue :: !Int,
    template   :: !Int,
    file :: !String}

main :: IO ()
main = do
    opts <- execParser optsParser
    let fileName = file opts 
    board <- generate (width opts) (height opts) (minvalue opts) (template opts)
    writeBoard board fileName
    
optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <> progDesc "Hidato Generator haskell implementation" <>
         header
             "Hidato - Project of haskell")
versionOption :: Parser (a -> a)
versionOption = infoOption "Hidato Generator Version 1.0" (long "version" <> help "Show version")
programOptions :: Parser Opts
programOptions =
    Opts <$> option auto
        (long "width" <> short 'w' <> showDefault <> metavar "INT" <> value 4 <>
         help "Board width")
    <*> option auto
        (long "height" <> short 'h' <> showDefault <> metavar "INT" <> value 4 <>
         help "Board height")
    <*> option auto
        (long "minvalue" <> short 'm' <> showDefault <> metavar "INT" <> value 1 <>
         help "Minimal value of a cell in the board")
    <*> option auto
        (long "template" <> short 't' <> showDefault <> metavar "INT" <> value 0 <>
         help "0: cloud\n1: mirror\n2: square")
    <*> strOption
        (long "file" <> short 'f' <> showDefault <> metavar "VALUE" <> value "board.txt" <>
         help "File to save")