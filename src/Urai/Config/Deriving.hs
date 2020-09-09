{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE TypeApplications        #-}

-- | Specification for Twitter Card metadata on websites.
module Urai.Config.Deriving
    ( LetBinding
    , MultiLetBound (..)
    , NamedBinding (..)
    , LetBound(..)
    , optionalTypeEncoder
    )
where

import qualified Data.Text as T
import Dhall (Encoder (..), FromDhall (..), GenericFromDhall, GenericToDhall, ToDhall (..),
              autoWith, defaultInterpretOptions, genericAutoWith, genericToDhallWith, inject)
import Dhall.Core (Expr (..), Var (V))
import Dhall.Deriving (ModifyOptions (..))
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data LetBinding (s :: Symbol)

class NamedBinding a where
  bindingName :: T.Text

instance KnownSymbol s => NamedBinding (LetBinding s) where
    bindingName = T.pack $ symbolVal @s Proxy

newtype MultiLetBound b m a = MultiLetBound { unMultiLetBound :: a }
  deriving stock (Generic, Functor)


instance ( Generic a
         , GenericFromDhall a (Rep a)
         , ModifyOptions m
         ) => FromDhall (MultiLetBound b m a) where
    autoWith _ = MultiLetBound
        <$> genericAutoWith (modifyOptions @m defaultInterpretOptions)


instance ( Generic a
         , GenericToDhall (Rep a)
         , ModifyOptions m
         ) => ToDhall (MultiLetBound b m a) where

    injectWith _ = unMultiLetBound >$< genericToDhallWith (modifyOptions @m defaultInterpretOptions)

instance ( NamedBinding b ) => NamedBinding (MultiLetBound m b a) where
  bindingName = (bindingName @b)



class LetBound a where
  typeEncoder :: Encoder a

instance {-# OVERLAPPABLE #-} (ToDhall a) => LetBound a where
    typeEncoder :: Encoder a
    typeEncoder = inject

instance {-# OVERLAPPING #-} ( ModifyOptions m
         , NamedBinding b
         , Generic a
         , GenericToDhall (Rep a)
         ) => LetBound (MultiLetBound b m a) where

    typeEncoder :: Encoder (MultiLetBound b m a)
    typeEncoder = Encoder { embed    = embed (inject @(MultiLetBound b m a))
                          , declared = declared' (bindingName @b)
                          }
        where declared' name = Var (V name 0)


optionalTypeEncoder :: (LetBound a) => Encoder (Maybe a)
optionalTypeEncoder = Encoder { embed = embed'
                              , declared = declared'
                              }
        where
          Encoder embedIn declaredIn = typeEncoder
          embed' (Just x) = Some (embedIn x)
          embed' Nothing  = App None declaredIn
          declared' = App Optional declaredIn
