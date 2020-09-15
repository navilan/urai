{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
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
    , printDhall
    )
where

import qualified Data.Text                     as T
import qualified Data.Tuple.Extra              as TE
import           Dhall                          ( Encoder(..)
                                                , FromDhall(..)
                                                , ToDhall(..)
                                                , auto
                                                , autoWith
                                                , constructor
                                                , encodeConstructorWith
                                                , encodeFieldWith
                                                , inject
                                                , recordEncoder
                                                , union
                                                , unionEncoder
                                                , (>*<)
                                                , (>|<)
                                                )
import           Dhall.Core                     ( Binding
                                                , Expr(..)
                                                , makeBinding
                                                , makeRecordField
                                                , pretty
                                                , variable
                                                , wrapInLets
                                                )
import           Dhall.Deriving                 ( type (<<<)
                                                , CamelCase
                                                , DropPrefix
                                                , Field
                                                )
import qualified Dhall.Map                     as DM
import           Dhall.Src                      ( Src )

import           Urai.Config.Deriving


data TwitterInfo = TwitterInfo
    { twitterInfoSite        :: T.Text
    , twitterInfoAuthor      :: T.Text
    , twitterInfoTitle       :: T.Text
    , twitterInfoDescription :: T.Text
    , twitterInfoImage       :: T.Text
    , twitterInfoImageAlt    :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall, LetBound) via MultiLetBound
        (LetBinding "TwitterInfo")
        (Field (CamelCase <<< DropPrefix "twitterInfo"))
        TwitterInfo


newtype TwitterSummaryCard = TwitterSummaryCard
    { twitterSummary :: TwitterInfo
    }
    deriving stock Generic
    deriving (FromDhall, LetBound) via MultiLetBound
        (LetBinding "TwitterSummaryCard")
        (Field (CamelCase <<< DropPrefix "twitter"))
        TwitterSummaryCard


injectTwitterSummaryCard :: Encoder TwitterSummaryCard
injectTwitterSummaryCard = recordEncoder $ contramap
    (\(TwitterSummaryCard i) -> i)
    (encodeFieldWith "summary" (typeEncoder @TwitterInfo))


instance ToDhall TwitterSummaryCard where
    injectWith _ = injectTwitterSummaryCard


newtype TwitterLargeCard = TwitterLargeCard
    { twitterLarge :: TwitterInfo
    }
    deriving stock Generic
    deriving (FromDhall, LetBound) via MultiLetBound
        (LetBinding "TwitterLargeCard")
        (Field (CamelCase <<< DropPrefix "twitter"))
        TwitterLargeCard

injectTwitterLargeCard :: Encoder TwitterLargeCard
injectTwitterLargeCard = recordEncoder $ contramap
    (\(TwitterLargeCard i) -> i)
    (encodeFieldWith "large" (typeEncoder @TwitterInfo))

instance ToDhall TwitterLargeCard where
    injectWith _ = injectTwitterLargeCard

data TwitterAppInfo = TwitterAppInfo
    { twitterAppName :: T.Text
    , twitterAppId   :: T.Text
    , twitterAppUrl  :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall, LetBound) via MultiLetBound
        (LetBinding "TwitterAppInfo")
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
    deriving (FromDhall, LetBound) via MultiLetBound
        (LetBinding "TwitterAppCard")
        (Field (CamelCase <<< DropPrefix "twitterApp"))
        TwitterAppCard


injectTwitterAppCard :: Encoder TwitterAppCard
injectTwitterAppCard = recordEncoder $ contramap
    (\(TwitterAppCard i c p pd a) -> (i, (c, (p, (pd, a)))))
    (   encodeFieldWith "info"    (typeEncoder @TwitterInfo)
    >*< encodeFieldWith "country" (typeEncoder @T.Text)
    >*< encodeFieldWith "iPhone"  (optionalTypeEncoder @TwitterAppInfo)
    >*< encodeFieldWith "iPad"    (optionalTypeEncoder @TwitterAppInfo)
    >*< encodeFieldWith "android" (optionalTypeEncoder @TwitterAppInfo)
    )

instance ToDhall TwitterAppCard where
    injectWith _ = injectTwitterAppCard

data TwitterPlayerCard = TwitterPlayerCard
    { twitterPlayerInfo      :: TwitterInfo
    , twitterPlayerPlayerUrl :: T.Text
    , twitterPlayerWidth     :: Integer
    , twitterPlayerHeight    :: Integer
    }
    deriving stock Generic
    deriving (FromDhall, LetBound) via MultiLetBound
        (LetBinding "TwitterPlayerCard")
        (Field (CamelCase <<< DropPrefix "twitterPlayer"))
        TwitterPlayerCard


injectTwitterPlayerCard :: Encoder TwitterPlayerCard
injectTwitterPlayerCard = recordEncoder $ contramap
    (\(TwitterPlayerCard i p w h) -> (i, (p, (w, h))))
    (   encodeFieldWith "info"   (typeEncoder @TwitterInfo)
    >*< encodeFieldWith "url"    (typeEncoder @T.Text)
    >*< encodeFieldWith "width"  (typeEncoder @Integer)
    >*< encodeFieldWith "height" (typeEncoder @Integer)
    )

instance ToDhall TwitterPlayerCard where
    injectWith _ = injectTwitterPlayerCard

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
    (   encodeConstructorWith "Summary" (typeEncoder @TwitterSummaryCard)
    >|< encodeConstructorWith "Large"   (typeEncoder @TwitterLargeCard)
    >|< encodeConstructorWith "App"     (typeEncoder @TwitterAppCard)
    >|< encodeConstructorWith "Player"  (typeEncoder @TwitterPlayerCard)
    )
  where
    adapt (SummaryCard x) = Left x
    adapt (LargeCard   x) = Right (Left x)
    adapt (AppCard     x) = Right (Right (Left x))
    adapt (PlayerCard  x) = Right (Right (Right x))

instance ToDhall TwitterCard where
    injectWith _ = injectTwitterCard


bindings :: [Binding Src Void]
bindings =
    [ makeBinding "TwitterInfo"        (declared (inject @TwitterInfo))
    , makeBinding "TwitterSummaryCard" (declared (inject @TwitterSummaryCard))
    , makeBinding "TwitterLargeCard"   (declared (inject @TwitterLargeCard))
    , makeBinding "TwitterAppInfo"     (declared (inject @TwitterAppInfo))
    , makeBinding "TwitterAppCard"     (declared (inject @TwitterAppCard))
    , makeBinding "TwitterPlayerCard"  (declared (inject @TwitterPlayerCard))
    , makeBinding "TwitterCard"        (declared (inject @TwitterCard))
    ]


expression :: [Binding Src a] -> Expr Src a
expression bs = RecordLit . DM.fromList $ map
    (variable TE.&&& (makeRecordField . Var . fromString . T.unpack . variable))
    bs

printDhall :: T.Text
printDhall = pretty $ wrapInLets bindings (expression bindings)
