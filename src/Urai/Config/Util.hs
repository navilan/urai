-- | Configuration utilities for generating Dhall types from Haskell
module Urai.Config.Util
  ( dropPrefix,
    printDhall,
  )
where

import Data.Either.Validation (validationToEither)
import qualified Data.Text as T
import Data.Text.Manipulate (toCamel)
import Dhall
import Dhall.Core
  ( Expr,
    pretty,
  )
import Dhall.Src (Src)


printDhall :: Expector (Expr Src Void) -> Either ExpectedTypeErrors T.Text
printDhall xp = case validationToEither xp of
  Left e -> Left e
  Right x -> Right $ pretty x

dropPrefix :: T.Text -> InterpretOptions
dropPrefix t =
  defaultInterpretOptions {fieldModifier = toCamel . T.drop (T.length t)}
