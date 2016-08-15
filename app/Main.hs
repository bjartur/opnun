module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Data.Function
import Data.List

type Address = String
type Hours = String
data Opening = Opening Hours Address

openings :: Scraper String [Opening]
openings = do
    chroots "td" opening

opening :: Scraper String Opening
opening = do
    directions <- text "h6"
    let address = directions & words & delete "kort" & unwords
    hours <- text "p"
    return (Opening hours address)

printOpening :: Opening -> IO ()
printOpening (Opening hours address) = do
    putStrLn address
    putStrLn ""
    putStrLn hours

printOpenings :: [Opening] -> IO ()
printOpenings = mapM_ printOpening

main :: IO ()
main = do
    elkoOpenings <- scrapeURL "https://www.elko.is/opnunartimi/" openings
    maybe (error "Opnunartímar Elko ófáanlegir.") printOpenings elkoOpenings
