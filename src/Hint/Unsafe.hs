
{-
    Find things that are unsafe

<TEST>
{-# NOINLINE slaves #-}; slaves = unsafePerformIO newIO
slaves = unsafePerformIO Multimap.newIO -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO Multimap.newIO
slaves = unsafePerformIO $ f y where foo = 1 -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO $ f y where foo = 1
slaves v = unsafePerformIO $ Multimap.newIO where foo = 1
slaves v = x where x = unsafePerformIO $ Multimap.newIO
slaves = x where x = unsafePerformIO $ Multimap.newIO -- {-# NOINLINE slaves #-} ; slaves = x where x = unsafePerformIO $ Multimap.newIO
slaves = unsafePerformIO . bar
slaves = unsafePerformIO . baz $ x -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO . baz $ x
slaves = unsafePerformIO . baz $ x -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO . baz $ x
</TEST>
-}


module Hint.Unsafe(unsafeHint) where

import Hint.Type(DeclHint',ModuleEx(..),Severity(..),rawIdea',toSS')
import Data.Char
import Refact.Types hiding(Match)
import Data.Generics.Uniplate.Operations

import HsSyn
import OccName
import RdrName
import FastString
import BasicTypes
import SrcLoc
import GHC.Util

-- The conditions on which to fire this hint are subtle. We are
-- interested exclusively in application constants involving
-- 'unsafePerformIO'. For example,
-- @
--   f = \x -> unsafePerformIO x
-- @
-- is not such a declaration (the right hand side is a lambda, not an
-- application) whereas,
-- @
--   f = g where g = unsafePerformIO Multimap.newIO
-- @
-- is. We advise that such constants should have a @NOINLINE@ pragma.
unsafeHint :: DeclHint'
unsafeHint _ (ModuleEx _ _ (L _ m) _) = \(L loc d) ->
  [rawIdea' Hint.Type.Warning "Missing NOINLINE pragma" loc
         (unsafePrettyPrint d)
         (Just $ dropWhile isSpace (unsafePrettyPrint $ gen x) ++ "\n" ++ unsafePrettyPrint d)
         [] [InsertComment (toSS' (L loc d)) (unsafePrettyPrint $ gen x)]
     -- 'x' does not declare a new function.
     | d@(ValD _
           FunBind {fun_id=L _ (Unqual x)
                      , fun_matches=MG{mg_origin=FromSource,mg_alts=L _ [L _ Match {m_pats=[]}]}}) <- [d]
     -- 'x' is a synonym for an appliciation involing 'unsafePerformIO'
     , isUnsafeDecl d
     -- 'x' is not marked 'NOINLINE'.
     , x `notElem` noinline]
  where
    gen :: OccName -> LHsDecl GhcPs
    gen x = noLoc $
      SigD noExt (InlineSig noExt (noLoc (mkRdrUnqual x))
                      (InlinePragma (SourceText "{-# NOINLINE") NoInline Nothing NeverActive FunLike))
    noinline :: [OccName]
    noinline = [q | LL _(SigD _ (InlineSig _ (L _ (Unqual q))
                                                (InlinePragma _ NoInline Nothing NeverActive FunLike))
        ) <- hsmodDecls m]

isUnsafeDecl :: HsDecl GhcPs -> Bool
isUnsafeDecl (ValD _ FunBind {fun_matches=MG {mg_origin=FromSource,mg_alts=LL _ alts}}) =
  any isUnsafeApp (childrenBi alts) || any isUnsafeDecl (childrenBi alts)
isUnsafeDecl _ = False

-- Am I equivalent to @unsafePerformIO x@?
isUnsafeApp :: HsExpr GhcPs -> Bool
isUnsafeApp (OpApp _ (LL _ l) op _ ) | isDol' op = isUnsafeFun l
isUnsafeApp (HsApp _ (LL _ x) _) = isUnsafeFun x
isUnsafeApp _ = False

-- Am I equivalent to @unsafePerformIO . x@?
isUnsafeFun :: HsExpr GhcPs -> Bool
isUnsafeFun (HsVar _ (LL _ x)) | x == mkVarUnqual (fsLit "unsafePerformIO") = True
isUnsafeFun (OpApp _ (LL _ l) op _) | isDot' op = isUnsafeFun l
isUnsafeFun _ = False
