module Main (main) where

import Urai (printDhall)

import qualified Data.Text.IO as TIO


main :: IO ()
main = TIO.writeFile "./TwitterCard.dhall" printDhall
