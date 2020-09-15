-- | A Dhall module (File) with multiple let bindings

module Urai.Config.Module
    ( Module
    , emptyModule
    , addBinding
    , evalModule
    )
where


import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc      ( Pretty )
import qualified Data.Tuple.Extra              as TE
import           Data.Tree                      ( unfoldTree
                                                , Tree
                                                )

import           Dhall.Core                     ( Binding
                                                , Expr(..)
                                                , RecordField(..)
                                                , Var(..)
                                                , makeBinding
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




type RecordExpanse = Tree Int

data ExpressionMeta a = ExpressionMeta
    { expression :: Expr Src a
    , expanse    :: RecordExpanse
    }

instance (Eq a, Pretty a, Show a) => Eq (ExpressionMeta a) where
    exp1 == exp2 =
        ((expanse exp1) == (expanse exp2))
            && (same $ diffNormalized (expression exp1) (expression exp2))


data ModuleBinding a = ModuleBinding
    { rawExp      :: ExpressionMeta a
    , substExp    :: ExpressionMeta a
    , bindingName :: Text
    }


asSubstExpr :: ModuleBinding a -> Expr Src a
asSubstExpr (ModuleBinding { substExp = ExpressionMeta {..} }) = expression


boundVar :: ModuleBinding a -> Expr Src a
boundVar ModuleBinding {..} = Var (V bindingName 0)


calculateExpanse' :: Maybe (Expr Src a) -> RecordExpanse
calculateExpanse' = unfoldTree expand
  where

    expand :: Maybe (Expr Src a) -> (Int, [Maybe (Expr Src a)])
    expand (Just (Record m)) = ((length m), expandRecordFields $ DM.sort m)
    expand (Just (Union  m)) = ((length m), expandUnionFields $ DM.sort m)
    expand _                 = (0, [])
    expandRecordFields :: DM.Map Text (RecordField s a) -> [Maybe (Expr s a)]
    expandRecordFields = map (Just . recordFieldValue) . DM.elems
    expandUnionFields :: DM.Map Text (Maybe (Expr s a)) -> [Maybe (Expr s a)]
    expandUnionFields = DM.elems


calculateExpanse :: Expr Src a -> RecordExpanse
calculateExpanse = calculateExpanse' . Just


makeMeta :: Expr Src a -> ExpressionMeta a
makeMeta x = ExpressionMeta { expression = x, expanse = calculateExpanse x }


makeModuleBinding :: Text -> Expr Src a -> ModuleBinding a
makeModuleBinding name e = ModuleBinding { rawExp      = xp
                                         , substExp    = xp
                                         , bindingName = name
                                         }
    where xp = makeMeta e

fromModuleBinding :: ModuleBinding a -> Binding Src a
fromModuleBinding ModuleBinding {..} =
    makeBinding bindingName (expression substExp)

type ModuleBindings = [ModuleBinding Void]
type Module a = StateT ModuleBindings Identity a

emptyModule :: ModuleBindings
emptyModule = []

addBinding :: Text -> Expr Src Void -> Module ()
addBinding t x = modify (<> (pure $ makeModuleBinding t x))


findBinding
    :: ExpressionMeta Void
    -> [ModuleBinding Void]
    -> Maybe (Expr Src Void)
findBinding meta =
    fmap boundVar . listToMaybe . filter (\b -> (rawExp b) == meta)


rewriteExpression
    :: Expr Src Void -> [ModuleBinding Void] -> Expr Src Void
rewriteExpression e@(Record _) = fromMaybe e . findBinding (makeMeta e)
rewriteExpression e            = const e

rewriteRecordField
    :: RecordField Src Void
    -> [ModuleBinding Void]
    -> RecordField Src Void
rewriteRecordField r@(RecordField {..}) bs =
    r { recordFieldValue = rewriteExpression recordFieldValue bs }


rewriteBinding
    :: ModuleBinding Void -> [ModuleBinding Void] -> ModuleBinding Void
rewriteBinding m bs = case asSubstExpr m of
    Record ms -> m
        { substExp = makeMeta . Record $ fmap (flip rewriteRecordField bs) ms
        }
    Union ms -> m
        { substExp = makeMeta . Union $ fmap
                         (fmap $ flip rewriteExpression bs)
                         ms
        }
    _ -> m


evalModule :: Module Text
evalModule = do
    bindings <- get
    let rewritten = toList $ fmap
            (fromModuleBinding . flip rewriteBinding bindings)
            bindings
    return . pretty $ wrapInLets rewritten (expression rewritten)
  where
    expression :: [Binding Src a] -> Expr Src a
    expression bs = RecordLit . DM.fromList $ map
        (      variable
        TE.&&& (makeRecordField . Var . fromString . T.unpack . variable)
        )
        bs
