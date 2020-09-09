-- | Writes the configuration types as Dhall files

module Urai.Config.ConfigWriter
    ( printDhall
    )
where

import           Data.Text                      ( pack )

-- import qualified Urai.Config.Meta              as M
import qualified Urai.Config.TwitterCard       as T
-- import qualified Urai.Config.OpenGraph         as O


printDhall :: Text
printDhall = T.printDhall
