{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
-- | Generic metadata for a website

module Urai.Config.Meta
    ( printDhall
    )
where

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

data Meta = Meta
    { metaDescription :: Text
    , metaAuthor      :: Text
    , metaTitle       :: Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "meta"))
        Meta


declareModule :: Module ()
declareModule = do
    addBinding "Meta" (declared (inject @Meta))

printDhall :: Module Text
printDhall = do
    declareModule
    evalModule
