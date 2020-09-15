{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- | Opengraph types. Not comprehensive yet. They cover the basic uses
-- for my own websites for now. Eventually, this will be a comprehensive
-- type system with all open graph options encoded.


module Urai.Config.OpenGraph
    ( OpenGraphSite(..)
    , OpenGraphBasic(..)
    , OpenGraphMediaInfo(..)
    , OpenGraphVisualMedia(..)
    , OpenGraphAudio(..)
    , OpenGraphImage(..)
    , OpenGraphVideo(..)
    , OpenGraphProfile(..)
    , OpenGraphArticle(..)
    , OpenGraphWebsite(..)
    , OpenGraphProfileItem(..)
    , OpenGraph(..)
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

data OpenGraphSite = OpenGraphSite
    { ogSiteLocale :: T.Text
    , ogSiteName   :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogSite"))
        OpenGraphSite


data OpenGraphBasic = OpenGraphBasic
    { ogBasicTitle :: T.Text
    , ogBasicImage :: T.Text
    , ogBasicUrl   :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogBasic"))
        OpenGraphBasic

data OpenGraphMediaInfo = OpenGraphMediaInfo
    { ogMediaContent     :: T.Text
    , ogMediaSecureUrl   :: T.Text
    , ogMediaContentType :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogMedia"))
        OpenGraphMediaInfo


data OpenGraphVisualMedia = OpenGraphVisualMedia
    { ogVisualMedia  :: OpenGraphMediaInfo
    , ogVisualWidth  :: Integer
    , ogVisualHeight :: Integer
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogVisual"))
        OpenGraphVisualMedia


newtype OpenGraphAudio = OpenGraphAudio
    { ogAudioMedia :: OpenGraphMediaInfo
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
    (Field (CamelCase <<< DropPrefix "ogAudio"))
    OpenGraphAudio

data OpenGraphImage = OpenGraphImage
    { ogImageVisual :: OpenGraphVisualMedia
    , ogImageAlt    :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogImage"))
        OpenGraphImage


newtype OpenGraphVideo = OpenGraphVideo
    { ogVideoVisual :: OpenGraphVisualMedia
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
    (Field (CamelCase <<< DropPrefix "ogVideo"))
    OpenGraphVideo

data OpenGraphProfile = OpenGraphProfile
    { ogProfileFirstName :: T.Text
    , ogProfileLastName  :: T.Text
    , ogProfileUserName  :: T.Text
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogProfile"))
        OpenGraphProfile

data OpenGraphArticle = OpenGraphArticle
    { ogArticlePublishedTime  :: T.Text
    , ogArticleModifiedTime   :: T.Text
    , ogArticleExpirationTime :: Maybe T.Text
    , ogArticleAuthors        :: [OpenGraphProfileItem]
    , ogArticleSection        :: T.Text
    , ogArticleTags           :: [T.Text]
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogArticle"))
        OpenGraphArticle


data OpenGraphWebsite = OpenGraphWebsite
    { ogWebsiteSite   :: OpenGraphSite
    , ogWebsiteBasic  :: OpenGraphBasic
    , ogWebsiteImages :: [OpenGraphImage]
    }
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec
        (Field (CamelCase <<< DropPrefix "ogWebsite"))
        OpenGraphWebsite


data OpenGraphProfileItem
  = OGProfileItem OpenGraphProfile
  | OGProfileUrl Text
    deriving stock Generic


instance FromDhall OpenGraphProfileItem where

    autoWith _ =
        union
            $  constructor "Item" (OGProfileItem <$> auto @OpenGraphProfile)
            <> constructor "Url"  auto


injectProfileItem :: Encoder OpenGraphProfileItem
injectProfileItem = adapt >$< unionEncoder
    (   encodeConstructorWith "Item" (inject @OpenGraphProfile)
    >|< encodeConstructorWith "Url"  (inject @Text)
    )
  where
    adapt (OGProfileItem x) = Left x
    adapt (OGProfileUrl  x) = Right x

instance ToDhall OpenGraphProfileItem where
    injectWith _ = injectProfileItem

data OpenGraph
  = OGWeb OpenGraphWebsite
  | OGArticle OpenGraphArticle
  | OGProfile OpenGraphProfile
  | OGVideo OpenGraphVideo
  | OGAudio OpenGraphAudio
  | OGImage OpenGraphImage
  deriving stock Generic


instance FromDhall OpenGraph where

    autoWith _ =
        union
            $  constructor "Web"     (OGWeb <$> auto @OpenGraphWebsite)
            <> constructor "Article" (OGArticle <$> auto @OpenGraphArticle)
            <> constructor "Profile" (OGProfile <$> auto @OpenGraphProfile)
            <> constructor "Video"   (OGVideo <$> auto @OpenGraphVideo)
            <> constructor "Audio"   (OGAudio <$> auto @OpenGraphAudio)
            <> constructor "Image"   (OGImage <$> auto @OpenGraphImage)


injectOpenGraph :: Encoder OpenGraph
injectOpenGraph = adapt >$< unionEncoder
    (   encodeConstructorWith "Web"     (inject @OpenGraphWebsite)
    >|< encodeConstructorWith "Article" (inject @OpenGraphArticle)
    >|< encodeConstructorWith "Profile" (inject @OpenGraphProfile)
    >|< encodeConstructorWith "Video"   (inject @OpenGraphVideo)
    >|< encodeConstructorWith "Audio"   (inject @OpenGraphAudio)
    >|< encodeConstructorWith "Image"   (inject @OpenGraphImage)
    )
  where
    adapt (OGWeb     x) = Left x
    adapt (OGArticle x) = Right (Left x)
    adapt (OGProfile x) = Right (Right (Left x))
    adapt (OGVideo   x) = Right (Right (Right (Left x)))
    adapt (OGAudio   x) = Right (Right (Right (Right (Left x))))
    adapt (OGImage   x) = Right (Right (Right (Right (Right x))))

instance ToDhall OpenGraph where
    injectWith _ = injectOpenGraph


declareModule :: Module ()
declareModule = do
    addBinding "OpenGraphSite"        (declared (inject @OpenGraphSite))
    addBinding "OpenGraphBasic"       (declared (inject @OpenGraphBasic))
    addBinding "OpenGraphMediaInfo"   (declared (inject @OpenGraphMediaInfo))
    addBinding "OpenGraphVisualMedia" (declared (inject @OpenGraphVisualMedia))
    addBinding "OpenGraphAudio"       (declared (inject @OpenGraphAudio))
    addBinding "OpenGraphImage"       (declared (inject @OpenGraphImage))
    addBinding "OpenGraphVideo"       (declared (inject @OpenGraphVideo))
    addBinding "OpenGraphProfile"     (declared (inject @OpenGraphProfile))
    addBinding "OpenGraphProfileItem" (declared (inject @OpenGraphProfileItem))
    addBinding "OpenGraphWebsite"     (declared (inject @OpenGraphWebsite))
    addBinding "OpenGraphArticle"     (declared (inject @OpenGraphArticle))
    addBinding "OpenGraph"            (declared (inject @OpenGraph))

printDhall :: Module Text
printDhall = do
    declareModule
    evalModule
