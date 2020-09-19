-- | A Dhall module (File) with multiple let bindings
{-# LANGUAGE StrictData #-}

module Urai.Config.Module
  ( Module
  , emptyModule
  , addBinding
  , addImport
  , evalModule
  )
where


import qualified Data.Text                     as T
import qualified Data.Tuple.Extra              as TE
import           Data.Tree                      ( unfoldTree
                                                , Tree
                                                )

import           Dhall.Core                     ( Binding
                                                , Expr(..)
                                                , Import
                                                , RecordField(..)
                                                , Var(..)
                                                , makeBinding
                                                , makeFieldSelection
                                                , makeRecordField
                                                , pretty
                                                , recordFieldValue
                                                , variable
                                                , wrapInLets
                                                )
import           Dhall.Diff                     ( Diff(..)
                                                , diffNormalized
                                                )
import qualified Dhall.Map                     as DM
import           Dhall.Src                      ( Src )


import           Unsafe.Coerce                  ( unsafeCoerce )




type RecordExpanse = Tree Int

data ExpressionMeta = ExpressionMeta
  { expression :: Expr Src Void
  , expanse    :: RecordExpanse
  }

instance Eq ExpressionMeta where
  exp1 == exp2 = (expanse exp1 == expanse exp2)
    && same (diffNormalized (expression exp1) (expression exp2))


data ExpressionBinding = ExpressionBinding
  { rawExp   :: ExpressionMeta
  , substExp :: ExpressionMeta
  , boundVar :: Expr Src Void
  }


data ImportBinding = ImportBinding
  { rawImport       :: [ExpressionBinding]
  , boundImport     :: Import
  , importQualifier :: Text
  }

data ModuleBinding
  = BoundExpression ExpressionBinding
  | ImportExpression ImportBinding


bindingName :: ExpressionBinding -> Text
bindingName b = case boundVar b of
  (Var (V name _)) -> name
  _others          -> "Unknown"


justExpression :: ModuleBinding -> Maybe ExpressionBinding
justExpression (BoundExpression x) = Just x
justExpression _import             = Nothing

justImport :: ModuleBinding -> Maybe ImportBinding
justImport (ImportExpression x) = Just x
justImport _exp                 = Nothing

type ModuleBindings = [ModuleBinding]
type Module a = StateT ModuleBindings Identity a


asSubstExpr :: ExpressionBinding -> Expr Src Void
asSubstExpr ExpressionBinding { substExp = ExpressionMeta {..} } = expression


calculateExpanse' :: Maybe (Expr Src Void) -> RecordExpanse
calculateExpanse' = unfoldTree expand
 where
  expand :: Maybe (Expr Src a) -> (Int, [Maybe (Expr Src a)])
  expand (Just (Record m)) = (length m, expandRecordFields $ DM.sort m)
  expand (Just (Union  m)) = (length m, expandUnionFields $ DM.sort m)
  expand _                 = (0, [])
  expandRecordFields :: DM.Map Text (RecordField s a) -> [Maybe (Expr s a)]
  expandRecordFields = map (Just . recordFieldValue) . DM.elems
  expandUnionFields :: DM.Map Text (Maybe (Expr s a)) -> [Maybe (Expr s a)]
  expandUnionFields = DM.elems


calculateExpanse :: Expr Src Void -> RecordExpanse
calculateExpanse = calculateExpanse' . Just


makeMeta :: Expr Src Void -> ExpressionMeta
makeMeta x = ExpressionMeta { expression = x, expanse = calculateExpanse x }


makeExpressionBinding :: Text -> Expr Src Void -> ExpressionBinding
makeExpressionBinding name e = ExpressionBinding { rawExp   = xp
                                                 , substExp = xp
                                                 , boundVar = Var (V name 0)
                                                 }
  where xp = makeMeta e


makeImportBinding :: Import -> Text -> [ModuleBinding] -> ImportBinding
makeImportBinding p n bs = ImportBinding { rawImport       = expressions bs
                                         , boundImport     = p
                                         , importQualifier = n
                                         }
 where
  expressions :: [ModuleBinding] -> [ExpressionBinding]
  expressions = fmap (makeQualifiedVar n) . mapMaybe justExpression
  makeQualifiedVar :: Text -> ExpressionBinding -> ExpressionBinding
  makeQualifiedVar qualifier e@ExpressionBinding {..} = e
    { boundVar = case boundVar of
                   (Var (V bName 0)) ->
                     Field (Var (V qualifier 0)) (makeFieldSelection bName)
                   x -> x
    }



emptyModule :: ModuleBindings
emptyModule = []

addBinding :: Text -> Expr Src Void -> Module ()
addBinding t x =
  modify (<> (pure . BoundExpression $ makeExpressionBinding t x))

addImport :: Import -> Text -> Module () -> Module ()
addImport p t m = do
  let bindings = execState m []
  modify (<> (pure . ImportExpression $ makeImportBinding p t bindings))


matchingBinding :: ExpressionMeta -> ModuleBinding -> Maybe (Expr Src Void)
matchingBinding meta (BoundExpression b) =
  boundVar <$> guarded ((== meta) . rawExp) b
matchingBinding meta (ImportExpression b) =
  findBinding meta (fmap BoundExpression (rawImport b))


findBinding :: ExpressionMeta -> [ModuleBinding] -> Maybe (Expr Src Void)
findBinding meta = find'ish (matchingBinding meta)
  where find'ish f = getAlt . foldMap' (Alt . f)


findBoundExpression :: Expr Src Void -> [ModuleBinding] -> Expr Src Void
findBoundExpression e = fromMaybe e . findBinding (makeMeta e)


rewriteExpression :: Expr Src Void -> [ModuleBinding] -> Expr Src Void
rewriteExpression e@(Record _      ) = findBoundExpression e
rewriteExpression e@(Union  _      ) = findBoundExpression e
rewriteExpression (  App List     e) = App List . findBoundExpression e
rewriteExpression (  App Optional e) = App Optional . findBoundExpression e
rewriteExpression e                  = const e

rewriteRecordField
  :: RecordField Src Void -> [ModuleBinding] -> RecordField Src Void
rewriteRecordField r@RecordField {..} bs =
  r { recordFieldValue = rewriteExpression recordFieldValue bs }


rewriteBinding :: ModuleBinding -> [ModuleBinding] -> ModuleBinding
rewriteBinding (BoundExpression m) bs = case asSubstExpr m of
  Record ms -> BoundExpression m
    { substExp = makeMeta . Record $ fmap (`rewriteRecordField` bs) ms
    }
  Union ms -> BoundExpression m
    { substExp = makeMeta . Union $ fmap (fmap $ flip rewriteExpression bs) ms
    }

  _otherExpressions -> BoundExpression m

rewriteBinding x _ = x



unsafeReinterpret :: Binding Src Void -> Binding Src Import
unsafeReinterpret = unsafeCoerce


unsafeFromModuleBinding :: ModuleBinding -> Binding Src Import
unsafeFromModuleBinding (BoundExpression e@ExpressionBinding {..}) =
  unsafeReinterpret $ makeBinding (bindingName e) (expression substExp)
unsafeFromModuleBinding (ImportExpression ImportBinding {..}) =
  makeBinding importQualifier (Embed boundImport)


evalModule :: Module Text
evalModule = do
  bindings <- get
  let exps        = BoundExpression <$> mapMaybe justExpression bindings
  let imps        = ImportExpression <$> mapMaybe justImport bindings
  let expressions = rewritten exps bindings
  let imports     = rewritten imps bindings
  let inner       = wrapInLets expressions (expression expressions)
  let outer       = wrapInLets imports inner
  return $ pretty outer
 where
  rewritten :: [ModuleBinding] -> [ModuleBinding] -> [Binding Src Import]
  rewritten bindings universe =
    fmap (unsafeFromModuleBinding . flip rewriteBinding universe) bindings
  expression :: [Binding Src Import] -> Expr Src Import
  expression bs = RecordLit . DM.fromList $ map
    (variable TE.&&& (makeRecordField . Var . fromString . T.unpack . variable))
    bs
