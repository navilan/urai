{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
-- | Social features for a urai site.

module Urai.Config.Social
    ( declareModule
    , printDhall
    )
where

import           System.FilePath                ( splitDirectories )

import           Dhall                          ( Encoder(..)
                                                , FromDhall(..)
                                                , ToDhall(..)
                                                , inject
                                                )

import           Dhall.Deriving                 ( type (<<<)
                                                , CamelCase
                                                , Codec(..)
                                                , DropPrefix
                                                , Field
                                                )

import           Urai.Config.Module
import qualified Urai.Config.TwitterCard       as TW
import qualified Urai.Config.OpenGraph         as OG
import           Dhall.Core                     ( Directory(..)
                                                , File(..)
                                                , Import(..)
                                                , ImportHashed(..)
                                                , ImportMode(..)
                                                , ImportType(..)
                                                , FilePrefix(..)
                                                )
import qualified Data.Text                     as T


data Social = Social
    { socialTwitter   :: TW.TwitterCard
    , socialOpenGraph :: OG.OpenGraph
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "social"))
        Social


fromPath :: Text -> File
fromPath p = File
    { directory = Directory . fmap T.pack $ fromMaybe
                      []
                      (viaNonEmpty tail comps)
    , file      = T.pack $ fromMaybe "" (viaNonEmpty head comps)
    }
  where
    comps =
        reverse . filter (/= "/") . splitDirectories $ toString p

makeImport :: FilePrefix -> Text -> Import
makeImport fp p = Import
    { importHashed = ImportHashed { hash       = Nothing
                                  , importType = Local fp (fromPath p)
                                  }
    , importMode   = Code
    }


declareModule :: Module ()
declareModule = do
    addImport (makeImport Here "TwitterCard.dhall") "T" TW.declareModule
    addImport (makeImport Here "OpenGraph.dhall")   "O" OG.declareModule
    addBinding "Social" (declared (inject @Social))


printDhall :: Module Text
printDhall = do
    declareModule
    evalModule
