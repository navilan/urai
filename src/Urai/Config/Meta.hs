{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
-- | Generic metadata for a website

module Urai.Config.Meta
    ( printDhall
    )
where

import qualified Data.Text                     as T
import           Dhall                          ( auto
                                                , Decoder
                                                , ExpectedTypeErrors
                                                , FromDhall(..)
                                                , expected
                                                , genericAutoWith
                                                )
import qualified Urai.Config.Util              as U

data Meta = Meta
    { metaDescription :: T.Text
    , metaAuthor      :: T.Text
    , metaTitle       :: T.Text
    }
    deriving stock Generic

instance FromDhall Meta where
    autoWith _ = genericAutoWith (U.dropPrefix "meta")


metaDecoder :: Decoder Meta
metaDecoder = auto

printDhall :: Either ExpectedTypeErrors T.Text
printDhall = U.printDhall (expected metaDecoder)
