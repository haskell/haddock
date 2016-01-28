{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haddock.Backends.Hyperlinker.Ast (enrich) where


import qualified Haddock.Syb as Syb
import Haddock.Backends.Hyperlinker.Types

import qualified GHC

import Control.Applicative
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Control.Monad
import qualified Data.Map.Strict as M
import Prelude hiding (concat, foldr, head, tail, replicate)
import qualified Data.List as List

everythingInRenamedSource :: (Alternative f, Data x)
  => (forall a. Data a => a -> f r) -> x -> f r
everythingInRenamedSource f = Syb.everythingButType @GHC.Name (<|>) f

-- | Add more detailed information to token stream using GHC API.
enrich :: GHC.RenamedSource -> [Token] -> [RichToken]
enrich src =
    map $ \token -> RichToken
        { rtkToken = token
        , rtkDetails = enrichToken token detailsMap
        }
  where
    detailsMap :: OldDetailsMap
    detailsMap = M.fromList . toList $ mconcat $ List.map ($ src)
        [ variables
        , types
        , decls
        , binds
        , imports
        ]

-- | A map containing association between source locations and "details" of
-- this location.
--
-- For the time being, it is just a list of pairs. However, looking up things
-- in such structure has linear complexity. We cannot use any hashmap-like
-- stuff because source locations are not ordered. In the future, this should
-- be replaced with interval tree data structure.
type DetailsMap = DList (GHC.SrcSpan, TokenDetails)
type OldDetailsMap = M.Map GHC.SrcSpan TokenDetails

lookupBySpan :: GHC.SrcSpan -> OldDetailsMap -> Maybe TokenDetails
lookupBySpan = M.lookup

enrichToken :: Token -> OldDetailsMap -> Maybe TokenDetails
enrichToken (Token typ _ spn) dm
    | typ `elem` [TkIdentifier, TkOperator] = lookupBySpan (GHC.RealSrcSpan spn) dm
enrichToken _ _ = Nothing

-- | Obtain details map for variables ("normally" used identifiers).
variables :: GHC.RenamedSource -> LTokenDetails
variables =
    everythingInRenamedSource (var `Syb.combine` rec)
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar name))) ->
            pure (sspan, RtkVar (GHC.unLoc name))
        (Just (GHC.L _ (GHC.RecordCon (GHC.L sspan name) _ _ _))) ->
            pure (sspan, RtkVar name)
        _ -> empty
    rec term = case cast term of
        Just (GHC.HsRecField (GHC.L sspan name) (_ :: GHC.LHsExpr GHC.Name) _) ->
            pure (sspan, RtkVar name)
        _ -> empty

-- | Obtain details map for types.
types :: GHC.RenamedSource -> LTokenDetails
types = everythingInRenamedSource ty
  where
    ty term = case cast term of
        (Just (GHC.L sspan (GHC.HsTyVar _ name))) ->
            pure (sspan, RtkType (GHC.unLoc name))
        _ -> empty

-- | Obtain details map for identifier bindings.
--
-- That includes both identifiers bound by pattern matching or declared using
-- ordinary assignment (in top-level declarations, let-expressions and where
-- clauses).

binds :: GHC.RenamedSource -> LTokenDetails
binds = everythingInRenamedSource
      (fun `Syb.combine` pat `Syb.combine` tvar)
  where
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ :: GHC.HsBind GHC.Name)) ->
            pure (sspan, RtkBind name)
        _ -> empty
    pat term = case cast term of
        (Just (GHC.L sspan (GHC.VarPat name))) ->
            pure (sspan, RtkBind (GHC.unLoc name))
        (Just (GHC.L _ (GHC.ConPatIn (GHC.L sspan name) recs))) ->
            (sspan, RtkVar name) `cons` everything (<|>) rec recs
        (Just (GHC.L _ (GHC.AsPat (GHC.L sspan name) _))) ->
            pure (sspan, RtkBind name)
        _ -> empty
    rec term = case cast term of
        (Just (GHC.HsRecField (GHC.L sspan name) (_ :: GHC.LPat GHC.Name) _)) ->
            pure (sspan, RtkVar name)
        _ -> empty
    tvar term = case cast term of
        (Just (GHC.L sspan (GHC.UserTyVar name))) ->
            pure (sspan, RtkBind (GHC.unLoc name))
        (Just (GHC.L _ (GHC.KindedTyVar (GHC.L sspan name) _))) ->
            pure (sspan, RtkBind name)
        _ -> empty

-- | Obtain details map for top-level declarations.
decls :: GHC.RenamedSource -> DetailsMap
decls (group, _, _, _) = mconcat $ map ($ group)
    [ mconcat . map typ . List.concat . map GHC.group_tyclds . GHC.hs_tyclds
    ,   everything (<|>) fun . GHC.hs_valds
    ,  everything (<|>) (con `combine` ins)
    ]
  where
    typ (GHC.L _ t) = case t of
        GHC.DataDecl { tcdLName = name } -> pure . decl $ name
        GHC.SynDecl name _ _ _ _ -> pure . decl $ name
        GHC.FamDecl fam -> pure . decl $ GHC.fdLName fam
        GHC.ClassDecl{..} -> decl tcdLName `cons` fromList (concatMap sig tcdSigs)
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ :: GHC.HsBind GHC.Name))
            | GHC.isExternalName name -> pure (sspan, RtkDecl name)
        _ -> empty
    con term = case cast term of
        (Just cdcl) ->
            fromList (map decl (GHC.getConNames cdcl)) <|> everything (<|>) fld cdcl
        Nothing -> empty
    ins term = case cast term of
        (Just (GHC.DataFamInstD inst)) -> pure . tyref $ GHC.dfid_tycon inst
        (Just (GHC.TyFamInstD (GHC.TyFamInstDecl (GHC.L _ eqn) _))) ->
            pure . tyref $ GHC.tfe_tycon eqn
        _ -> empty
    fld term = case cast term of
        Just (field :: GHC.ConDeclField GHC.Name)
          -> fromList (map (decl . fmap GHC.selectorFieldOcc) $ GHC.cd_fld_names field)
        Nothing -> empty
    sig (GHC.L _ (GHC.TypeSig names _)) = map decl names
    sig _ = []
    decl (GHC.L sspan name) = (sspan, RtkDecl name)
    tyref (GHC.L sspan name) = (sspan, RtkType name)

-- | Obtain details map for import declarations.
--
-- This map also includes type and variable details for items in export and
-- import lists.
imports :: GHC.RenamedSource -> LTokenDetails
imports src@(_, imps, _, _) =
    everything (<|>) ie src <> fromList (mapMaybe (imp . GHC.unLoc) imps)
  where
    ie term = case cast term of
        (Just (GHC.IEVar v)) -> pure $ var $ GHC.ieLWrappedName v
        (Just (GHC.IEThingAbs t)) -> pure $ typ $ GHC.ieLWrappedName t
        (Just (GHC.IEThingAll t)) -> pure $ typ $ GHC.ieLWrappedName t
        (Just (GHC.IEThingWith t _ vs _fls)) ->
          typ t `cons` fromList (map var vs)
        _ -> empty
    typ (GHC.L sspan name) = (sspan, RtkType name)
    var (GHC.L sspan name) = (sspan, RtkVar name)
    imp idecl | not . GHC.ideclImplicit $ idecl =
        let (GHC.L sspan name) = GHC.ideclName idecl
        in Just (sspan, RtkModule name)
    imp _ = Nothing


newtype DList a = DL { unDL :: [a] -> [a] }

-- | Convert a list to a dlist
fromList    :: [a] -> DList a
fromList    = DL . (++)
{-# INLINE fromList #-}

-- | Convert a dlist to a list
toList      :: DList a -> [a]
toList      = ($[]) . unDL
{-# INLINE toList #-}

-- | Apply a dlist to a list to get the underlying list with an extension
--
-- > apply (fromList xs) ys = xs ++ ys
apply       :: DList a -> [a] -> [a]
apply       = unDL

-- | Create a dlist containing no elements
emptyD       :: DList a
emptyD       = DL id
{-# INLINE emptyD #-}

-- | Create dlist with a single element
singleton   :: a -> DList a
singleton   = DL . (:)
{-# INLINE singleton #-}

-- | /O(1)/. Prepend a single element to a dlist
infixr `cons`
cons        :: a -> DList a -> DList a
cons x xs   = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- | /O(1)/. Append a single element to a dlist
infixl `snoc`
snoc        :: DList a -> a -> DList a
snoc xs x   = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- | /O(1)/. Append dlists
append       :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

-- | /O(spine)/. Concatenate dlists
concat       :: [DList a] -> DList a
concat       = List.foldr append empty
{-# INLINE concat #-}

-- | /O(n)/. Create a dlist of the given number of elements
replicate :: Int -> a -> DList a
replicate n x = DL $ \xs -> let go m | m <= 0    = xs
                                     | otherwise = x : go (m-1)
                            in go n
{-# INLINE replicate #-}

-- | /O(n)/. List elimination for dlists
list :: b -> (a -> DList a -> b) -> DList a -> b
list nill consit dl =
  case toList dl of
    [] -> nill
    (x : xs) -> consit x (fromList xs)

-- | /O(n)/. Return the head of the dlist
head :: DList a -> a
head = list (error "Data.DList.head: empty dlist") const

-- | /O(n)/. Return the tail of the dlist
tail :: DList a -> DList a
tail = list (error "Data.DList.tail: empty dlist") (flip const)

-- | /O(n)/. Unfoldr for dlists
unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr pf b =
  case pf b of
    Nothing     -> empty
    Just (a, b') -> cons a (unfoldr pf b')

-- | /O(n)/. Foldr over difference lists
foldr        :: (a -> b -> b) -> b -> DList a -> b
foldr f b    = List.foldr f b . toList
{-# INLINE foldr #-}

-- | /O(n)/. Map over difference lists.
mapD          :: (a -> b) -> DList a -> DList b
mapD f        = foldr (cons . f) empty
{-# INLINE mapD #-}

instance Monoid (DList a) where
    mempty  = emptyD
    mappend = append

instance Functor DList where
    fmap = mapD
    {-# INLINE fmap #-}

instance Applicative DList where
    pure  = return
    (<*>) = ap

instance Alternative DList where
    empty = empty
    (<|>) = append

instance Monad DList where
  m >>= k
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    = foldr (append . k) empty m
  {-# INLINE (>>=) #-}

  return x = singleton x
  {-# INLINE return #-}

  fail _   = empty
