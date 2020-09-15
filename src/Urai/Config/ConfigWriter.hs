-- | Writes the configuration types as Dhall files

module Urai.Config.ConfigWriter
    ( printDhall
    )
where

-- import           Data.Text                      ( pack )

-- import qualified Urai.Config.Meta              as M
-- import qualified Urai.Config.TwitterCard       as T
import qualified Urai.Config.OpenGraph         as O
import           Urai.Config.Module


printDhall :: Text
printDhall = evalState O.printDhall emptyModule
