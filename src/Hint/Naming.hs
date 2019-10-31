{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-
    Suggest the use of camelCase

    Only permit:
    _*[A-Za-z]*_*#*'*

    Apply this to things that would get exported by default only
    Also allow prop_ as it's a standard QuickCheck idiom
    Also allow case_ as it's a standard test-framework-th idiom
    Also allow test_ as it's a standard tasty-th idiom
    Also allow numbers separated by _
    Also don't suggest anything mentioned elsewhere in the module
    Don't suggest for FFI, since they match their C names

<TEST>
data Yes = Foo | Bar'Test -- data Yes = Foo | BarTest
data Yes = Bar | Test_Bar -- data Yes = Bar | TestBar
data No = a :::: b
data Yes = Foo {bar_cap :: Int}
data No = FOO | BarBAR | BarBBar
yes_foo = yes_foo + yes_foo -- yesFoo = ...
yes_fooPattern Nothing = 0 -- yesFooPattern Nothing = ...
no = 1 where yes_foo = 2
a -== b = 1
myTest = 1; my_test = 1
semiring'laws = 1 -- semiringLaws = ...
data Yes = FOO_A | Foo_B -- data Yes = FOO_A | FooB
case_foo = 1
test_foo = 1
cast_foo = 1 -- castFoo = ...
replicateM_ = 1
_foo__ = 1
section_1_1 = 1
runMutator# = 1
foreign import ccall hexml_node_child :: IO ()
</TEST>
-}


module Hint.Naming(namingHint) where

import Hint.Type (Idea,DeclHint',suggest',isSym,toSrcSpan',ghcModule)
import Data.Generics.Uniplate.Operations
import Data.List.Extra (nubOrd, isPrefixOf)
import Data.Data
import Data.Char
import Data.Maybe
import Refact.Types hiding (RType(Match))
import qualified Data.Set as Set

import BasicTypes
import FastString
import HsDecls
import HsExtension
import HsSyn
import OccName
import SrcLoc

import GHC.Util

namingHint :: DeclHint'
namingHint _ modu = naming $ Set.fromList $ concatMap getNames $ hsmodDecls $ unLoc (ghcModule modu)

naming :: Set.Set String -> LHsDecl GhcPs -> [Idea]
naming seen originalDecl =
    [ suggest' "Use camelCase"
               (shorten originalDecl)
               (shorten replacedDecl)
               [Replace Bind (toSrcSpan' originalDecl) [] (unsafePrettyPrint replacedDecl)]
    | not $ null suggestedNames
    ]
    where
        suggestedNames =
            [ (originalName, suggestedName)
            | not $ isForD' originalDecl
            , originalName <- nubOrd $ getNames originalDecl
            , Just suggestedName <- [suggestName originalName]
            , not $ suggestedName `Set.member` seen
            ]
        replacedDecl = replaceNames suggestedNames originalDecl

shorten :: LHsDecl GhcPs -> LHsDecl GhcPs
shorten (LL locDecl (ValD ttg0 bind@(FunBind _ _ matchGroup@(MG _ (LL locMatches matches) FromSource) _ _))) =
    LL locDecl (ValD ttg0 bind {fun_matches = matchGroup {mg_alts = LL locMatches $ map shortenMatch matches}})
shorten (LL locDecl (ValD ttg0 bind@(PatBind _ _ grhss@(GRHSs _ rhss _) _))) =
    LL locDecl (ValD ttg0 bind {pat_rhs = grhss {grhssGRHSs = map shortenLGRHS rhss}})
shorten x = x

shortenMatch :: LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
shortenMatch (LL locMatch match@(Match _ _ _ _ grhss@(GRHSs _ rhss _))) =
    LL locMatch match {m_grhss = grhss {grhssGRHSs = map shortenLGRHS rhss}}
shortenMatch x = x

shortenLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs)
shortenLGRHS (LL locGRHS (GRHS ttg0 guards (LL locExpr _))) =
    LL locGRHS (GRHS ttg0 guards (cL locExpr dots))
    where
        dots :: HsExpr GhcPs
        dots = HsLit NoExt (HsString (SourceText "...") (mkFastString "..."))
shortenLGRHS x = x

getNames :: LHsDecl GhcPs -> [String]
getNames decl = maybeToList (declName decl) ++ getConstructorNames (unLoc decl)

getConstructorNames :: HsDecl GhcPs -> [String]
getConstructorNames (TyClD _ (DataDecl _ _ _ _ (HsDataDefn _ _ _ _ _ cons _))) =
    concatMap (map unsafePrettyPrint . getConNames . unLoc) cons
getConstructorNames _ = []

suggestName :: String -> Maybe String
suggestName original
    | isSym original || good || not (any isLower original) || any isDigit original ||
        any (`isPrefixOf` original) ["prop_","case_","unit_","test_","spec_","scprop_","hprop_"] = Nothing
    | otherwise = Just $ f original
    where
        good = all isAlphaNum $ drp '_' $ drp '#' $ drp '\'' $ reverse $ drp '_' original
        drp x = dropWhile (== x)

        f xs = us ++ g ys
            where (us,ys) = span (== '_') xs

        g x | x `elem` ["_","'","_'"] = x
        g (a:x:xs) | a `elem` "_'" && isAlphaNum x = toUpper x : g xs
        g (x:xs) | isAlphaNum x = x : g xs
                 | otherwise = g xs
        g [] = []

replaceNames :: Data a => [(String, String)] -> a -> a
replaceNames rep = transformBi replace
    where
        replace :: OccName -> OccName
        replace (unsafePrettyPrint -> name) = mkOccName srcDataName $ fromMaybe name $ lookup name rep
