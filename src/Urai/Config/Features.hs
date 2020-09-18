-- | Set of configuration options supported for the site

module Urai.Config.Features
  ( Social
  , SocialFeatures
  )
where

data Social = Twitter
            | OpenGraph
            deriving stock (Show, Eq, Ord)


newtype SocialFeatures = SocialFeatures [Social] deriving newtype (Show, Eq, Ord)
