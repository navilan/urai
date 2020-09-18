-- | Writes the configuration types as Dhall files

module Urai.Config.ConfigWriter
  ( printDhall
  )
where

import qualified Data.Text.IO                  as TIO
import           Development.Shake.FilePath     ( (</>) )

import qualified Urai.Config.Meta              as M
import qualified Urai.Config.OpenGraph         as O
import qualified Urai.Config.TwitterCard       as T
import qualified Urai.Config.Social            as S
import           Urai.Config.Module


printModule :: FilePath -> FilePath -> Module Text -> IO ()
printModule d f m = do
  let txt = evalState m emptyModule
  TIO.writeFile (d </> f) txt


printDhall :: FilePath -> IO ()
printDhall p = do
  printModule p "Meta.dhall"        M.printDhall
  printModule p "OpenGraph.dhall"   O.printDhall
  printModule p "TwitterCard.dhall" T.printDhall
  printModule p "Social.dhall"      S.printDhall
