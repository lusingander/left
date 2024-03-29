module Main where

import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative

import Left

data Options = Options { number :: Int } deriving (Show)

numberP :: Parser Int
numberP = option auto $ mconcat
    [ short 'n'
    , long "number"
    , value 10
    , metavar "N"
    , showDefault
    ]

optionsP :: Parser Options
optionsP = (<*>) helper $
    Options <$> numberP

parserInfo :: ParserInfo Options
parserInfo = info optionsP $ mconcat []

run :: Options -> IO ()
run (Options n) = interact $ lefts n

main :: IO ()
main = do
    options <- execParser parserInfo
    run options
