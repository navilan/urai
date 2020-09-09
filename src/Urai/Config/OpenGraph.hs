-- {-# LANGUAGE DeriveAnyClass #-}
-- -- | Opengraph types. Not comprehensive yet. They cover the basic uses
-- -- for my own websites for now. Eventually, this will be a comprehensive
-- -- type system with all open graph options encoded.


module Urai.Config.OpenGraph where


-- import           GHC.Generics
-- import qualified Data.Text                     as T
-- import           Dhall                          ( auto
--                                                 , expected
--                                                 , Interpret
--                                                 )
-- import           Dhall.Core                     ( pretty )

-- data OpenGraphSite = OpenGraphSite
--     { locale   :: T.Text
--     , siteName :: T.Text
--     }
--     deriving (Generic, Interpret)

-- data OpenGraphBasic = OpenGraphBasic
--     { title :: T.Text
--     , image :: T.Text
--     , url   :: T.Text
--     }
--     deriving (Generic, Interpret)

-- data OpenGraphMediaInfo = OpenGraphMediaInfo
--     { content     :: T.Text
--     , secureUrl   :: T.Text
--     , contentType :: T.Text
--     }
--     deriving (Generic, Interpret)


-- data OpenGraphVisualMedia = OpenGraphVisualMedia
--     { info   :: OpenGraphMediaInfo
--     , width  :: Integer
--     , height :: Integer
--     }
--     deriving (Generic, Interpret)


-- data OpenGraphAudio = OpenGraphAudio
--     { info :: OpenGraphMediaInfo
--     }
--     deriving (Generic, Interpret)

-- data OpenGraphImage = OpenGraphImage
--     { media :: OpenGraphVisualMedia
--     , alt   :: T.Text
--     }
--     deriving (Generic, Interpret)


-- data OpenGraphVideo = OpenGraphVideo
--     { media :: OpenGraphVisualMedia
--     }
--     deriving (Generic, Interpret)

-- data OpenGraphProfile = OpenGraphProfile
--     { firstName :: T.Text
--     , lastName  :: T.Text
--     , userName  :: T.Text
--     }
--     deriving (Generic, Interpret)

-- data OpenGraphArticle = OpenGraphArticle
--     { publishedTime  :: T.Text
--     , modifiedTime   :: T.Text
--     , expirationTime :: Maybe T.Text
--     , authors        :: [OpenGraphProfileItem]
--     , section        :: T.Text
--     , tags           :: [T.Text]
--     }
--     deriving (Generic, Interpret)


-- data OpenGraphWebsite = OpenGraphWebsite
--     { site   :: OpenGraphSite
--     , basic  :: OpenGraphBasic
--     , images :: [OpenGraphIimage]
--     }
--     deriving (Generic, Interpret)


-- data OpenGraphProfileItem
--   = OpenGraphProfile
--   | T.Text
--     deriving (Generic, Interpret)

-- data OpenGraph
--   = OpenGraphWebsite
--   | OpenGraphArticle
--   | OpenGraphProfile
--   | OpenGraphVideo
--   | OpenGraphAudio
--   | OpenGraphImage
--   deriving stock (Generic, Interpret)

-- toText :: Text
-- toText = pretty (expected (auto @OpenGraph))
