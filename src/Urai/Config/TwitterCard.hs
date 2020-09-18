{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- | Specification for Twitter Card metadata on websites.
module Urai.Config.TwitterCard
    ( TwitterInfo(..)
    , TwitterSummaryCard(..)
    , TwitterLargeCard(..)
    , TwitterAppCard(..)
    , TwitterPlayerCard(..)
    , TwitterCard(..)
    , declareModule
    , printDhall
    )
where

import qualified Data.Text                     as T
import           Dhall                          ( Encoder(..)
                                                , FromDhall(..)
                                                , ToDhall(..)
                                                , auto
                                                , autoWith
                                                , constructor
                                                , encodeConstructorWith
                                                , inject
                                                , union
                                                , unionEncoder
                                                , (>|<)
                                                )
import           Dhall.Deriving                 ( type (<<<)
                                                , CamelCase
                                                , Codec(..)
                                                , DropPrefix
                                                , Field
                                                )

import           Urai.Config.Module


data TwitterInfo = TwitterInfo
    { twitterInfoSite        :: T.Text
    , twitterInfoAuthor      :: T.Text
    , twitterInfoTitle       :: T.Text
    , twitterInfoDescription :: T.Text
    , twitterInfoImage       :: T.Text
    , twitterInfoImageAlt    :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "twitterInfo"))
        TwitterInfo


newtype TwitterSummaryCard = TwitterSummaryCard
    { twitterSummary :: TwitterInfo
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "twitter"))
        TwitterSummaryCard


newtype TwitterLargeCard = TwitterLargeCard
    { twitterLarge :: TwitterInfo
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "twitter"))
        TwitterLargeCard

data TwitterAppInfo = TwitterAppInfo
    { twitterAppName :: T.Text
    , twitterAppId   :: T.Text
    , twitterAppUrl  :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "twitterApp"))
        TwitterAppInfo

data TwitterAppCard = TwitterAppCard
    { twitterAppInfo    :: TwitterInfo
    , twitterAppCountry :: T.Text
    , twitterAppiPhone  :: Maybe TwitterAppInfo
    , twitterAppiPad    :: Maybe TwitterAppInfo
    , twitterAppAndroid :: Maybe TwitterAppInfo
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "twitterApp"))
        TwitterAppCard

data TwitterPlayerCard = TwitterPlayerCard
    { twitterPlayerInfo      :: TwitterInfo
    , twitterPlayerPlayerUrl :: T.Text
    , twitterPlayerWidth     :: Integer
    , twitterPlayerHeight    :: Integer
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "twitterPlayer"))
        TwitterPlayerCard


data TwitterCard
  = SummaryCard TwitterSummaryCard
  | LargeCard TwitterLargeCard
  | AppCard TwitterAppCard
  | PlayerCard TwitterPlayerCard
  deriving stock Generic


instance FromDhall TwitterCard where

    autoWith _ =
        union
            $  constructor "Summary" (SummaryCard <$> auto @TwitterSummaryCard)
            <> constructor "Large"   (LargeCard <$> auto @TwitterLargeCard)
            <> constructor "App"     (AppCard <$> auto @TwitterAppCard)
            <> constructor "Player"  (PlayerCard <$> auto @TwitterPlayerCard)


injectTwitterCard :: Encoder TwitterCard
injectTwitterCard = adapt >$< unionEncoder
    (   encodeConstructorWith "Summary" (inject @TwitterSummaryCard)
    >|< encodeConstructorWith "Large"   (inject @TwitterLargeCard)
    >|< encodeConstructorWith "App"     (inject @TwitterAppCard)
    >|< encodeConstructorWith "Player"  (inject @TwitterPlayerCard)
    )
  where
    adapt (SummaryCard x) = Left x
    adapt (LargeCard   x) = Right (Left x)
    adapt (AppCard     x) = Right (Right (Left x))
    adapt (PlayerCard  x) = Right (Right (Right x))

instance ToDhall TwitterCard where
    injectWith _ = injectTwitterCard


declareModule :: Module ()
declareModule = do
    addBinding "TwitterInfo"        (declared (inject @TwitterInfo))
    addBinding "TwitterSummaryCard" (declared (inject @TwitterSummaryCard))
    addBinding "TwitterLargeCard"   (declared (inject @TwitterLargeCard))
    addBinding "TwitterAppInfo"     (declared (inject @TwitterAppInfo))
    addBinding "TwitterAppCard"     (declared (inject @TwitterAppCard))
    addBinding "TwitterPlayerCard"  (declared (inject @TwitterPlayerCard))
    addBinding "TwitterCard"        (declared (inject @TwitterCard))



printDhall :: Module Text
printDhall = do
  declareModule
  evalModule
