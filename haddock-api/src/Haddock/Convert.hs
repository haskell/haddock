{-# LANGUAGE CPP, PatternGuards, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Convert
-- Copyright   :  (c) Isaac Dupree 2009,
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversion between TyThing and HsDecl. This functionality may be moved into
-- GHC at some point.
-----------------------------------------------------------------------------
module Haddock.Convert (
  tyThingToLHsDecl,
  synifyInstHead,
  synifyFamInst,
  PrintRuntimeReps(..),
) where

import Bag ( emptyBag )
import BasicTypes ( TupleSort(..), SourceText(..), LexicalFixity(..)
                  , PromotionFlag(..), DefMethSpec(..) )
import Class
import CoAxiom
import ConLike
import Data.Either (lefts, rights)
import DataCon
import FamInstEnv
import HsSyn
import Name
import NameSet ( emptyNameSet )
import RdrName ( mkVarUnqual )
import PatSyn
import SrcLoc ( Located, noLoc, unLoc, GenLocated(..), srcLocSpan )
import TcType
import TyCon
import Type
import TyCoRep
import TysPrim ( alphaTyVars )
import TysWiredIn ( eqTyConName, listTyConName, liftedTypeKindTyConName
                  , unitTy, promotedNilDataCon, promotedConsDataCon )
import PrelNames ( hasKey, eqTyConKey, ipClassKey, tYPETyConKey
                 , liftedRepDataConKey )
import Unique ( getUnique )
import Util ( chkAppend,dropList, filterByList, filterOut, splitAtList )
import Var
import VarSet

import Haddock.Types
import Haddock.Interface.Specialize
import Haddock.GhcUtils                      ( orderedFVs, defaultRuntimeRepVars )

import Data.Maybe                            ( catMaybes, maybeToList )


-- | Whether or not to default 'RuntimeRep' variables to 'LiftedRep'. Check
-- out Note [Defaulting RuntimeRep variables] in IfaceType.hs for the
-- motivation.
data PrintRuntimeReps = ShowRuntimeRep | HideRuntimeRep deriving Show

-- the main function here! yay!
tyThingToLHsDecl
  :: PrintRuntimeReps
  -> TyThing
  -> Either ErrMsg ([ErrMsg], (HsDecl GhcRn))
tyThingToLHsDecl prr t = case t of
  -- ids (functions and zero-argument a.k.a. CAFs) get a type signature.
  -- Including built-in functions like seq.
  -- foreign-imported functions could be represented with ForD
  -- instead of SigD if we wanted...
  --
  -- in a future code version we could turn idVarDetails = foreign-call
  -- into a ForD instead of a SigD if we wanted.  Haddock doesn't
  -- need to care.
  AnId i -> allOK $ SigD noExt (synifyIdSig prr ImplicitizeForAll [] i)

  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ATyCon tc
    | Just cl <- tyConClass_maybe tc -- classes are just a little tedious
    -> let extractFamilyDecl :: TyClDecl a -> Either ErrMsg (FamilyDecl a)
           extractFamilyDecl (FamDecl _ d) = return d
           extractFamilyDecl _           =
             Left "tyThingToLHsDecl: impossible associated tycon"

           extractFamDefDecl :: FamilyDecl GhcRn -> Type -> TyFamDefltEqn GhcRn
           extractFamDefDecl fd rhs = FamEqn
             { feqn_ext = noExt
             , feqn_tycon = fdLName fd
             , feqn_bndrs  = Nothing
                 -- TODO: this must change eventually
             , feqn_pats = fdTyVars fd
             , feqn_fixity = fdFixity fd
             , feqn_rhs = synifyType WithinType [] rhs }

           extractAtItem
             :: ClassATItem
             -> Either ErrMsg (LFamilyDecl GhcRn, Maybe (LTyFamDefltEqn GhcRn))
           extractAtItem (ATI at_tc def) = do
             tyDecl <- synifyTyCon prr Nothing at_tc
             famDecl <- extractFamilyDecl tyDecl
             let defEqnTy = fmap (noLoc . extractFamDefDecl famDecl . fst) def
             pure (noLoc famDecl, defEqnTy)

           atTyClDecls = map extractAtItem (classATItems cl)
           (atFamDecls, atDefFamDecls) = unzip (rights atTyClDecls)
           vs = tyConVisibleTyVars (classTyCon cl)

       in withErrs (lefts atTyClDecls) . TyClD noExt $ ClassDecl
         { tcdCtxt = synifyCtx (classSCTheta cl)
         , tcdLName = synifyName cl
         , tcdTyVars = synifyTyVars vs
         , tcdFixity = synifyFixity cl
         , tcdFDs = map (\ (l,r) -> noLoc
                        (map (noLoc . getName) l, map (noLoc . getName) r) ) $
                         snd $ classTvsFds cl
         , tcdSigs = noLoc (MinimalSig noExt NoSourceText . noLoc . fmap noLoc $ classMinimalDef cl) :
                      [ noLoc tcdSig
                      | clsOp <- classOpItems cl
                      , tcdSig <- synifyTcIdSig vs clsOp ]
         , tcdMeths = emptyBag --ignore default method definitions, they don't affect signature
         -- class associated-types are a subset of TyCon:
         , tcdATs = atFamDecls
         , tcdATDefs = catMaybes atDefFamDecls
         , tcdDocs = [] --we don't have any docs at this point
         , tcdCExt = placeHolderNamesTc }
    | otherwise
    -> synifyTyCon prr Nothing tc >>= allOK . TyClD noExt

  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  ACoAxiom ax -> synifyAxiom ax >>= allOK

  -- a data-constructor alone just gets rendered as a function:
  AConLike (RealDataCon dc) -> allOK $ SigD noExt (TypeSig noExt [synifyName dc]
    (synifySigWcType ImplicitizeForAll [] (dataConUserType dc)))

  AConLike (PatSynCon ps) ->
    allOK . SigD noExt $ PatSynSig noExt [synifyName ps] (synifyPatSynSigType ps)
  where
    withErrs e x = return (e, x)
    allOK x = return (mempty, x)

synifyAxBranch :: TyCon -> CoAxBranch -> TyFamInstEqn GhcRn
synifyAxBranch tc (CoAxBranch { cab_tvs = tkvs, cab_lhs = args, cab_rhs = rhs })
  = let name            = synifyName tc
        args_types_only = filterOutInvisibleTypes tc args
        typats          = map (synifyType WithinType []) args_types_only
        annot_typats    = zipWith3 annotHsType (mkIsPolyTvs fam_tvs)
                                   args_types_only typats
        hs_rhs          = synifyType WithinType [] rhs
    in HsIB { hsib_ext = map tyVarName tkvs
            , hsib_body   = FamEqn { feqn_ext    = noExt
                                   , feqn_tycon  = name
                                   , feqn_bndrs  = Nothing
                                       -- TODO: this must change eventually
                                   , feqn_pats   = map HsValArg annot_typats
                                   , feqn_fixity = synifyFixity name
                                   , feqn_rhs    = hs_rhs } }
  where
    fam_tvs = tyConVisibleTyVars tc

synifyAxiom :: CoAxiom br -> Either ErrMsg (HsDecl GhcRn)
synifyAxiom ax@(CoAxiom { co_ax_tc = tc })
  | isOpenTypeFamilyTyCon tc
  , Just branch <- coAxiomSingleBranch_maybe ax
  = return $ InstD noExt
           $ TyFamInstD noExt
           $ TyFamInstDecl { tfid_eqn = synifyAxBranch tc branch }

  | Just ax' <- isClosedSynFamilyTyConWithAxiom_maybe tc
  , getUnique ax' == getUnique ax   -- without the getUniques, type error
  = synifyTyCon ShowRuntimeRep (Just ax) tc >>= return . TyClD noExt

  | otherwise
  = Left "synifyAxiom: closed/open family confusion"

-- | Turn type constructors into data declarations, type families, or type synonyms
synifyTyCon
  :: PrintRuntimeReps
  -> Maybe (CoAxiom br)  -- ^ RHS of type synonym
  -> TyCon               -- ^ type constructor to convert
  -> Either ErrMsg (TyClDecl GhcRn)
synifyTyCon prr _coax tc
  | isFunTyCon tc || isPrimTyCon tc
  = return $
    DataDecl { tcdLName = synifyName tc
             , tcdTyVars = HsQTvs { hsq_ext =
                                       HsQTvsRn { hsq_implicit = []   -- No kind polymorphism
                                                , hsq_dependent = emptyNameSet }
                                   , hsq_explicit = zipWith mk_hs_tv
                                                            tyVarKinds
                                                            alphaTyVars --a, b, c... which are unfortunately all kind *
                                   }

           , tcdFixity = synifyFixity tc

           , tcdDataDefn = HsDataDefn { dd_ext = noExt
                                      , dd_ND = DataType  -- arbitrary lie, they are neither
                                                    -- algebraic data nor newtype:
                                      , dd_ctxt = noLoc []
                                      , dd_cType = Nothing
                                      , dd_kindSig = synifyDataTyConReturnKind tc
                                               -- we have their kind accurately:
                                      , dd_cons = []  -- No constructors
                                      , dd_derivs = noLoc [] }
           , tcdDExt = DataDeclRn False placeHolderNamesTc }
  where
    -- tyConTyVars doesn't work on fun/prim, but we can make them up:
    mk_hs_tv realKind fakeTyVar
      | isLiftedTypeKind realKind = noLoc $ UserTyVar noExt (noLoc (getName fakeTyVar))
      | otherwise = noLoc $ KindedTyVar noExt (noLoc (getName fakeTyVar)) (synifyKindSig realKind)

    conKind = defaultType prr (tyConKind tc)
    tyVarKinds = fst . splitFunTys . snd . splitPiTysInvisible $ conKind

synifyTyCon _prr _coax tc
  | Just flav <- famTyConFlav_maybe tc
  = case flav of
      -- Type families
      OpenSynFamilyTyCon -> mkFamDecl OpenTypeFamily
      ClosedSynFamilyTyCon mb
        | Just (CoAxiom { co_ax_branches = branches }) <- mb
          -> mkFamDecl $ ClosedTypeFamily $ Just
            $ map (noLoc . synifyAxBranch tc) (fromBranches branches)
        | otherwise
          -> mkFamDecl $ ClosedTypeFamily $ Just []
      BuiltInSynFamTyCon {}
        -> mkFamDecl $ ClosedTypeFamily $ Just []
      AbstractClosedSynFamilyTyCon {}
        -> mkFamDecl $ ClosedTypeFamily Nothing
      DataFamilyTyCon {}
        -> mkFamDecl DataFamily
  where
    resultVar = famTcResVar tc
    mkFamDecl i = return $ FamDecl noExt $
      FamilyDecl { fdExt = noExt
                 , fdInfo = i
                 , fdLName = synifyName tc
                 , fdTyVars = synifyTyVars (tyConVisibleTyVars tc)
                 , fdFixity = synifyFixity tc
                 , fdResultSig =
                       synifyFamilyResultSig resultVar (tyConResKind tc)
                 , fdInjectivityAnn =
                       synifyInjectivityAnn  resultVar (tyConTyVars tc)
                                       (tyConInjectivityInfo tc)
                 }

synifyTyCon _prr coax tc
  | Just ty <- synTyConRhs_maybe tc
  = return $ SynDecl { tcdSExt   = emptyNameSet
                     , tcdLName  = synifyName tc
                     , tcdTyVars = synifyTyVars (tyConVisibleTyVars tc)
                     , tcdFixity = synifyFixity tc
                     , tcdRhs = synifyType WithinType [] ty }
  | otherwise =
  -- (closed) newtype and data
  let
  alg_nd = if isNewTyCon tc then NewType else DataType
  alg_ctx = synifyCtx (tyConStupidTheta tc)
  name = case coax of
    Just a -> synifyName a -- Data families are named according to their
                           -- CoAxioms, not their TyCons
    _ -> synifyName tc
  tyvars = synifyTyVars (tyConVisibleTyVars tc)
  kindSig = synifyDataTyConReturnKind tc
  -- The data constructors.
  --
  -- Any data-constructors not exported from the module that *defines* the
  -- type will not (cannot) be included.
  --
  -- Very simple constructors, Haskell98 with no existentials or anything,
  -- probably look nicer in non-GADT syntax.  In source code, all constructors
  -- must be declared with the same (GADT vs. not) syntax, and it probably
  -- is less confusing to follow that principle for the documentation as well.
  --
  -- There is no sensible infix-representation for GADT-syntax constructor
  -- declarations.  They cannot be made in source code, but we could end up
  -- with some here in the case where some constructors use existentials.
  -- That seems like an acceptable compromise (they'll just be documented
  -- in prefix position), since, otherwise, the logic (at best) gets much more
  -- complicated. (would use dataConIsInfix.)
  use_gadt_syntax = isGadtSyntaxTyCon tc
  consRaw = map (synifyDataCon use_gadt_syntax) (tyConDataCons tc)
  cons = rights consRaw
  -- "deriving" doesn't affect the signature, no need to specify any.
  alg_deriv = noLoc []
  defn = HsDataDefn { dd_ext     = noExt
                    , dd_ND      = alg_nd
                    , dd_ctxt    = alg_ctx
                    , dd_cType   = Nothing
                    , dd_kindSig = kindSig
                    , dd_cons    = cons
                    , dd_derivs  = alg_deriv }
 in case lefts consRaw of
  [] -> return $
        DataDecl { tcdLName = name, tcdTyVars = tyvars
                 , tcdFixity = synifyFixity name
                 , tcdDataDefn = defn
                 , tcdDExt = DataDeclRn False placeHolderNamesTc }
  dataConErrs -> Left $ unlines dataConErrs

-- | In this module, every TyCon being considered has come from an interface
-- file. This means that when considering a data type constructor such as:
--
-- > data Foo (w :: *) (m :: * -> *) (a :: *)
--
-- Then its tyConKind will be (* -> (* -> *) -> * -> *). But beware! We are
-- also rendering the type variables of Foo, so if we synify the tyConKind of
-- Foo in full, we will end up displaying this in Haddock:
--
-- > data Foo (w :: *) (m :: * -> *) (a :: *)
-- >   :: * -> (* -> *) -> * -> *
--
-- Which is entirely wrong (#548). We only want to display the /return/ kind,
-- which this function obtains.
synifyDataTyConReturnKind :: TyCon -> Maybe (LHsKind GhcRn)
synifyDataTyConReturnKind tc
  | isLiftedTypeKind ret_kind = Nothing -- Don't bother displaying :: *
  | otherwise                 = Just (synifyKindSig ret_kind)
  where ret_kind = tyConResKind tc

synifyInjectivityAnn :: Maybe Name -> [TyVar] -> Injectivity
                     -> Maybe (LInjectivityAnn GhcRn)
synifyInjectivityAnn Nothing _ _            = Nothing
synifyInjectivityAnn _       _ NotInjective = Nothing
synifyInjectivityAnn (Just lhs) tvs (Injective inj) =
    let rhs = map (noLoc . tyVarName) (filterByList inj tvs)
    in Just $ noLoc $ InjectivityAnn (noLoc lhs) rhs

synifyFamilyResultSig :: Maybe Name -> Kind -> LFamilyResultSig GhcRn
synifyFamilyResultSig  Nothing    kind
   | isLiftedTypeKind kind = noLoc $ NoSig noExt
   | otherwise = noLoc $ KindSig  noExt (synifyKindSig kind)
synifyFamilyResultSig (Just name) kind =
   noLoc $ TyVarSig noExt (noLoc $ KindedTyVar noExt (noLoc name) (synifyKindSig kind))

-- User beware: it is your responsibility to pass True (use_gadt_syntax)
-- for any constructor that would be misrepresented by omitting its
-- result-type.
-- But you might want pass False in simple enough cases,
-- if you think it looks better.
synifyDataCon :: Bool -> DataCon -> Either ErrMsg (LConDecl GhcRn)
synifyDataCon use_gadt_syntax dc =
 let
  -- dataConIsInfix allegedly tells us whether it was declared with
  -- infix *syntax*.
  use_infix_syntax = dataConIsInfix dc
  use_named_field_syntax = not (null field_tys)
  name = synifyName dc
  -- con_qvars means a different thing depending on gadt-syntax
  (_univ_tvs, ex_tvs, _eq_spec, theta, arg_tys, res_ty) = dataConFullSig dc
  user_tvs = dataConUserTyVars dc -- Used for GADT data constructors

  -- skip any EqTheta, use 'orig'inal syntax
  ctx | null theta = Nothing
      | otherwise = Just $ synifyCtx theta

  linear_tys =
    zipWith (\ty bang ->
               let tySyn = synifyType WithinType [] ty
               in case bang of
                    (HsSrcBang _ NoSrcUnpack NoSrcStrict) -> tySyn
                    bang' -> noLoc $ HsBangTy noExt bang' tySyn)
            arg_tys (dataConSrcBangs dc)

  field_tys = zipWith con_decl_field (dataConFieldLabels dc) linear_tys
  con_decl_field fl synTy = noLoc $
    ConDeclField noExt [noLoc $ FieldOcc (flSelector fl) (noLoc $ mkVarUnqual $ flLabel fl)] synTy
                 Nothing
  hs_arg_tys = case (use_named_field_syntax, use_infix_syntax) of
          (True,True) -> Left "synifyDataCon: contradiction!"
          (True,False) -> return $ RecCon (noLoc field_tys)
          (False,False) -> return $ PrefixCon linear_tys
          (False,True) -> case linear_tys of
                           [a,b] -> return $ InfixCon a b
                           _ -> Left "synifyDataCon: infix with non-2 args?"
 -- finally we get synifyDataCon's result!
 in hs_arg_tys >>=
      \hat ->
        if use_gadt_syntax
           then return $ noLoc $
              ConDeclGADT { con_g_ext  = noExt
                          , con_names  = [name]
                          , con_forall = noLoc $ not $ null user_tvs
                          , con_qvars  = synifyTyVars user_tvs
                          , con_mb_cxt = ctx
                          , con_args   = hat
                          , con_res_ty = synifyType WithinType [] res_ty
                          , con_doc    = Nothing }
           else return $ noLoc $
              ConDeclH98 { con_ext    = noExt
                         , con_name   = name
                         , con_forall = noLoc False
                         , con_ex_tvs = map synifyTyVar ex_tvs
                         , con_mb_cxt = ctx
                         , con_args   = hat
                         , con_doc    = Nothing }

synifyName :: NamedThing n => n -> Located Name
synifyName n = L (srcLocSpan (getSrcLoc n)) (getName n)

-- | Guess the fixity of a something with a name. This isn't quite right, since
-- a user can always declare an infix name in prefix form or a prefix name in
-- infix form. Unfortunately, that is not something we can usually reconstruct.
synifyFixity :: NamedThing n => n -> LexicalFixity
synifyFixity n | isSymOcc (getOccName n) = Infix
               | otherwise = Prefix

synifyIdSig
  :: PrintRuntimeReps -- ^ are we printing tyvars of kind 'RuntimeRep'?
  -> SynifyTypeState  -- ^ what to do with a 'forall'
  -> [TyVar]          -- ^ free variables in the type to convert
  -> Id               -- ^ the 'Id' from which to get the type signature
  -> Sig GhcRn
synifyIdSig prr s vs i = TypeSig noExt [synifyName i] (synifySigWcType s vs t)
  where
    t = defaultType prr (varType i)

-- | Turn a 'ClassOpItem' into a list of signatures. The list returned is going
-- to contain the synified 'ClassOpSig' as well (when appropriate) a default
-- 'ClassOpSig'.
synifyTcIdSig :: [TyVar] -> ClassOpItem -> [Sig GhcRn]
synifyTcIdSig vs (i, dm) =
  [ ClassOpSig noExt False [synifyName i] (mainSig (varType i)) ] ++
  [ ClassOpSig noExt True [noLoc dn] (defSig dt)
  | Just (dn, GenericDM dt) <- [dm] ]
  where
    mainSig t = synifySigType DeleteTopLevelQuantification vs t
    defSig t = synifySigType ImplicitizeForAll vs t

synifyCtx :: [PredType] -> LHsContext GhcRn
synifyCtx = noLoc . map (synifyType WithinType [])


synifyTyVars :: [TyVar] -> LHsQTyVars GhcRn
synifyTyVars ktvs = HsQTvs { hsq_ext = HsQTvsRn { hsq_implicit = []
                                                , hsq_dependent = emptyNameSet }
                           , hsq_explicit = map synifyTyVar ktvs }

synifyTyVar :: TyVar -> LHsTyVarBndr GhcRn
synifyTyVar = synifyTyVar' emptyVarSet

-- | Like 'synifyTyVar', but accepts a set of variables for which to omit kind
-- signatures (even if they don't have the lifted type kind).
synifyTyVar' :: VarSet -> TyVar -> LHsTyVarBndr GhcRn
synifyTyVar' no_kinds tv
  | isLiftedTypeKind kind || tv `elemVarSet` no_kinds
  = noLoc (UserTyVar noExt (noLoc name))
  | otherwise = noLoc (KindedTyVar noExt (noLoc name) (synifyKindSig kind))
  where
    kind = tyVarKind tv
    name = getName tv


-- | Annotate (with HsKingSig) a type if the first parameter is True
-- and if the type contains a free variable.
-- This is used to synify type patterns for poly-kinded tyvars in
-- synifying class and type instances.
annotHsType :: Bool   -- True <=> annotate
            -> Type -> LHsType GhcRn -> LHsType GhcRn
  -- tiny optimization: if the type is annotated, don't annotate again.
annotHsType _    _  hs_ty@(L _ (HsKindSig {})) = hs_ty
annotHsType True ty hs_ty
  | not $ isEmptyVarSet $ filterVarSet isTyVar $ tyCoVarsOfType ty
  = let ki    = typeKind ty
        hs_ki = synifyType WithinType [] ki
    in noLoc (HsKindSig noExt hs_ty hs_ki)
annotHsType _    _ hs_ty = hs_ty

-- | For every type variable in the input,
-- report whether or not the tv is poly-kinded. This is used to eventually
-- feed into 'annotHsType'.
mkIsPolyTvs :: [TyVar] -> [Bool]
mkIsPolyTvs = map is_poly_tv
  where
    is_poly_tv tv = not $
                    isEmptyVarSet $
                    filterVarSet isTyVar $
                    tyCoVarsOfType $
                    tyVarKind tv

--states of what to do with foralls:
data SynifyTypeState
  = WithinType
  -- ^ normal situation.  This is the safe one to use if you don't
  -- quite understand what's going on.
  | ImplicitizeForAll
  -- ^ beginning of a function definition, in which, to make it look
  --   less ugly, those rank-1 foralls (without kind annotations) are made
  --   implicit.
  | DeleteTopLevelQuantification
  -- ^ because in class methods the context is added to the type
  --   (e.g. adding @forall a. Num a =>@ to @(+) :: a -> a -> a@)
  --   which is rather sensible,
  --   but we want to restore things to the source-syntax situation where
  --   the defining class gets to quantify all its functions for free!


synifySigType :: SynifyTypeState -> [TyVar] -> Type -> LHsSigType GhcRn
-- The empty binders is a bit suspicious;
-- what if the type has free variables?
synifySigType s vs ty = mkEmptyImplicitBndrs (synifyType s vs ty)

synifySigWcType :: SynifyTypeState -> [TyVar] -> Type -> LHsSigWcType GhcRn
-- Ditto (see synifySigType)
synifySigWcType s vs ty = mkEmptyWildCardBndrs (mkEmptyImplicitBndrs (synifyType s vs ty))

synifyPatSynSigType :: PatSyn -> LHsSigType GhcRn
-- Ditto (see synifySigType)
synifyPatSynSigType ps = mkEmptyImplicitBndrs (synifyPatSynType ps)

-- | Depending on the first argument, try to default all type variables of kind
-- 'RuntimeRep' to 'LiftedType'.
defaultType :: PrintRuntimeReps -> Type -> Type
defaultType ShowRuntimeRep = id
defaultType HideRuntimeRep = defaultRuntimeRepVars

-- | Convert a core type into an 'HsType'.
synifyType
  :: SynifyTypeState  -- ^ what to do with a 'forall'
  -> [TyVar]          -- ^ free variables in the type to convert
  -> Type             -- ^ the type to convert
  -> LHsType GhcRn
synifyType _ _ (TyVarTy tv) = noLoc $ HsTyVar noExt NotPromoted $ noLoc (getName tv)
synifyType _ vs (TyConApp tc tys)
  = maybe_sig res_ty
  where
    res_ty :: LHsType GhcRn
    res_ty
      -- Use */# instead of TYPE 'Lifted/TYPE 'Unlifted (#473)
      | tc `hasKey` tYPETyConKey
      , [TyConApp lev []] <- tys
      , lev `hasKey` liftedRepDataConKey
      = noLoc (HsTyVar noExt NotPromoted (noLoc liftedTypeKindTyConName))
      -- Use non-prefix tuple syntax where possible, because it looks nicer.
      | Just sort <- tyConTuple_maybe tc
      , tyConArity tc == tys_len
      = noLoc $ HsTupleTy noExt
                          (case sort of
                              BoxedTuple      -> HsBoxedTuple
                              ConstraintTuple -> HsConstraintTuple
                              UnboxedTuple    -> HsUnboxedTuple)
                           (map (synifyType WithinType vs) vis_tys)
      | isUnboxedSumTyCon tc = noLoc $ HsSumTy noExt (map (synifyType WithinType vs) vis_tys)
      | Just dc <- isPromotedDataCon_maybe tc
      , isTupleDataCon dc
      , dataConSourceArity dc == length vis_tys
      = noLoc $ HsExplicitTupleTy noExt (map (synifyType WithinType vs) vis_tys)
      -- ditto for lists
      | getName tc == listTyConName, [ty] <- vis_tys =
         noLoc $ HsListTy noExt (synifyType WithinType vs ty)
      | tc == promotedNilDataCon, [] <- vis_tys
      = noLoc $ HsExplicitListTy noExt IsPromoted []
      | tc == promotedConsDataCon
      , [ty1, ty2] <- vis_tys
      = let hTy = synifyType WithinType vs ty1
        in case synifyType WithinType vs ty2 of
             tTy | L _ (HsExplicitListTy _ IsPromoted tTy') <- stripKindSig tTy
                 -> noLoc $ HsExplicitListTy noExt IsPromoted (hTy : tTy')
                 | otherwise
                 -> noLoc $ HsOpTy noExt hTy (noLoc $ getName tc) tTy
      -- ditto for implicit parameter tycons
      | tc `hasKey` ipClassKey
      , [name, ty] <- tys
      , Just x <- isStrLitTy name
      = noLoc $ HsIParamTy noExt (noLoc $ HsIPName x) (synifyType WithinType vs ty)
      -- and equalities
      | tc `hasKey` eqTyConKey
      , [ty1, ty2] <- tys
      = noLoc $ HsOpTy noExt
                       (synifyType WithinType vs ty1)
                       (noLoc eqTyConName)
                       (synifyType WithinType vs ty2)
      -- and infix type operators
      | isSymOcc (nameOccName (getName tc))
      , ty1:ty2:tys_rest <- vis_tys
      = mk_app_tys (HsOpTy noExt
                           (synifyType WithinType vs ty1)
                           (noLoc $ getName tc)
                           (synifyType WithinType vs ty2))
                   tys_rest
      -- Most TyCons:
      | otherwise
      = mk_app_tys (HsTyVar noExt prom $ noLoc (getName tc))
                   vis_tys
      where
        prom = if isPromotedDataCon tc then IsPromoted else NotPromoted
        mk_app_tys ty_app ty_args =
          foldl (\t1 t2 -> noLoc $ HsAppTy noExt t1 t2)
                (noLoc ty_app)
                (map (synifyType WithinType vs) $
                 filterOut isCoercionTy ty_args)

    tys_len = length tys
    vis_tys = filterOutInvisibleTypes tc tys

    maybe_sig :: LHsType GhcRn -> LHsType GhcRn
    maybe_sig ty'
      | tyConAppNeedsKindSig False tc tys_len
      = let full_kind  = typeKind (mkTyConApp tc tys)
            full_kind' = synifyType WithinType vs full_kind
        in noLoc $ HsKindSig noExt ty' full_kind'
      | otherwise = ty'

synifyType s vs (AppTy t1 (CoercionTy {})) = synifyType s vs t1
synifyType _ vs (AppTy t1 t2) = let
  s1 = synifyType WithinType vs t1
  s2 = synifyType WithinType vs t2
  in noLoc $ HsAppTy noExt s1 s2
synifyType s vs funty@(FunTy t1 t2)
  | isPredTy t1 = synifyForAllType s vs funty
  | otherwise = let s1 = synifyType WithinType vs t1
                    s2 = synifyType WithinType vs t2
                in noLoc $ HsFunTy noExt s1 s2
synifyType s vs forallty@(ForAllTy _tv _ty) = synifyForAllType s vs forallty

synifyType _ _ (LitTy t) = noLoc $ HsTyLit noExt $ synifyTyLit t
synifyType s vs (CastTy t _) = synifyType s vs t
synifyType _ _ (CoercionTy {}) = error "synifyType:Coercion"

-- | Process a 'Type' which starts with a forall or a constraint into
-- an 'HsType'
synifyForAllType
  :: SynifyTypeState  -- ^ what to do with the 'forall'
  -> [TyVar]          -- ^ free variables in the type to convert
  -> Type             -- ^ the forall type to convert
  -> LHsType GhcRn
synifyForAllType s vs ty =
  let (tvs, ctx, tau) = tcSplitSigmaTyPreserveSynonyms ty
      sPhi = HsQualTy { hst_ctxt = synifyCtx ctx
                      , hst_xqual = noExt
                      , hst_body = synifyType WithinType (tvs' ++ vs) tau }

      sTy = HsForAllTy { hst_bndrs = sTvs
                       , hst_xforall = noExt
                       , hst_body  = noLoc sPhi }

      sTvs = map synifyTyVar tvs

      -- Figure out what the type variable order would be inferred in the
      -- absence of an explicit forall
      tvs' = orderedFVs (mkVarSet vs) (ctx ++ [tau])

  in case s of
    DeleteTopLevelQuantification -> synifyType ImplicitizeForAll (tvs' ++ vs) tau

    -- Put a forall in if there are any type variables
    WithinType
      | not (null tvs) -> noLoc sTy
      | otherwise -> noLoc sPhi

    ImplicitizeForAll -> implicitForAll [] vs tvs ctx (synifyType WithinType) tau


-- | Put a forall in if there are any type variables which require
-- explicit kind annotations or if the inferred type variable order
-- would be different.
implicitForAll
  :: [TyCon]          -- ^ type constructors that determine their args kinds
  -> [TyVar]          -- ^ free variables in the type to convert
  -> [TyVar]          -- ^ type variable binders in the forall
  -> ThetaType        -- ^ constraints right after the forall
  -> ([TyVar] -> Type -> LHsType GhcRn) -- ^ how to convert the inner type
  -> Type             -- ^ inner type
  -> LHsType GhcRn
implicitForAll tycons vs tvs ctx synInner tau
  | any (isHsKindedTyVar . unLoc) sTvs = noLoc sTy
  | tvs' /= tvs                        = noLoc sTy
  | otherwise                          = noLoc sPhi
  where
  sRho = synInner (tvs' ++ vs) tau
  sPhi | null ctx = unLoc sRho
       | otherwise
       = HsQualTy { hst_ctxt = synifyCtx ctx
                  , hst_xqual = noExt
                  , hst_body = synInner (tvs' ++ vs) tau }
  sTy = HsForAllTy { hst_bndrs = sTvs
                   , hst_xforall = noExt
                   , hst_body = noLoc sPhi }

  no_kinds_needed = noKindTyVars tycons tau
  sTvs = map (synifyTyVar' no_kinds_needed) tvs

  -- Figure out what the type variable order would be inferred in the
  -- absence of an explicit forall
  tvs' = orderedFVs (mkVarSet vs) (ctx ++ [tau])



-- | Find the set of type variables whose kind signatures can be properly
-- inferred just from their uses in the type signature. This means the type
-- variable to has at least one fully applied use @f x1 x2 ... xn@ where:
--
--   * @f@ has a function kind where the arguments have the same kinds
--     as @x1 x2 ... xn@.
--
--   * @f@ has a function kind whose final return has lifted type kind
--
noKindTyVars
  :: [TyCon]  -- ^ type constructors that determine their args kinds
  -> Type     -- ^ type to inspect
  -> VarSet   -- ^ set of variables whose kinds can be inferred from uses in the type
noKindTyVars _ (TyVarTy var)
  | isLiftedTypeKind (tyVarKind var) = unitVarSet var
noKindTyVars ts ty
  | (f, xs) <- splitAppTys ty
  , not (null xs)
  = let args = map (noKindTyVars ts) xs
        func = case f of
                 TyVarTy var | (xsKinds, outKind) <- splitFunTys (tyVarKind var)
                             , xsKinds `eqTypes` map typeKind xs
                             , isLiftedTypeKind outKind
                             -> unitVarSet var
                 TyConApp t ks | t `elem` ts
                               , all noFreeVarsOfType ks
                               -> mkVarSet [ v | TyVarTy v <- xs ]
                 _ -> noKindTyVars ts f
    in unionVarSets (func : args)
noKindTyVars ts (ForAllTy _ t) = noKindTyVars ts t
noKindTyVars ts (FunTy t1 t2) = noKindTyVars ts t1 `unionVarSet` noKindTyVars ts t2
noKindTyVars ts (CastTy t _) = noKindTyVars ts t
noKindTyVars _ _ = emptyVarSet

synifyPatSynType :: PatSyn -> LHsType GhcRn
synifyPatSynType ps =
  let (univ_tvs, req_theta, ex_tvs, prov_theta, arg_tys, res_ty) = patSynSig ps
      ts = maybeToList (tyConAppTyCon_maybe res_ty)

      -- HACK: a HsQualTy with theta = [unitTy] will be printed as "() =>",
      -- i.e., an explicit empty context, which is what we need. This is not
      -- possible by taking theta = [], as that will print no context at all
      req_theta' | null req_theta
                 , not (null prov_theta && null ex_tvs)
                 = [unitTy]
                 | otherwise = req_theta

  in implicitForAll ts [] (univ_tvs ++ ex_tvs) req_theta'
       (\vs -> implicitForAll ts vs [] prov_theta (synifyType WithinType))
       (mkFunTys arg_tys res_ty)

synifyTyLit :: TyLit -> HsTyLit
synifyTyLit (NumTyLit n) = HsNumTy NoSourceText n
synifyTyLit (StrTyLit s) = HsStrTy NoSourceText s

synifyKindSig :: Kind -> LHsKind GhcRn
synifyKindSig k = synifyType WithinType [] k

stripKindSig :: LHsType GhcRn -> LHsType GhcRn
stripKindSig (L _ (HsKindSig _ t _)) = t
stripKindSig t = t

synifyInstHead :: ([TyVar], [PredType], Class, [Type]) -> InstHead GhcRn
synifyInstHead (vs, preds, cls, types) = specializeInstHead $ InstHead
    { ihdClsName = getName cls
    , ihdTypes = map unLoc annot_ts
    , ihdInstType = ClassInst
        { clsiCtx = map (unLoc . synifyType WithinType []) preds
        , clsiTyVars = synifyTyVars (tyConVisibleTyVars cls_tycon)
        , clsiSigs = map synifyClsIdSig $ classMethods cls
        , clsiAssocTys = do
            (Right (FamDecl _ fam)) <- map (synifyTyCon HideRuntimeRep Nothing)
                                           (classATs cls)
            pure $ mkPseudoFamilyDecl fam
        }
    }
  where
    cls_tycon = classTyCon cls
    ts  = filterOutInvisibleTypes cls_tycon types
    ts' = map (synifyType WithinType vs) ts
    annot_ts = zipWith3 annotHsType is_poly_tvs ts ts'
    is_poly_tvs = mkIsPolyTvs (tyConVisibleTyVars cls_tycon)
    synifyClsIdSig = synifyIdSig ShowRuntimeRep DeleteTopLevelQuantification vs

-- Convert a family instance, this could be a type family or data family
synifyFamInst :: FamInst -> Bool -> Either ErrMsg (InstHead GhcRn)
synifyFamInst fi opaque = do
    ityp' <- ityp fam_flavor
    return InstHead
        { ihdClsName = fi_fam fi
        , ihdTypes = map unLoc annot_ts
        , ihdInstType = ityp'
        }
  where
    ityp SynFamilyInst | opaque = return $ TypeInst Nothing
    ityp SynFamilyInst =
        return . TypeInst . Just . unLoc $ synifyType WithinType [] fam_rhs
    ityp (DataFamilyInst c) =
        DataInst <$> synifyTyCon HideRuntimeRep (Just $ famInstAxiom fi) c
    fam_tc     = famInstTyCon fi
    fam_flavor = fi_flavor fi
    fam_lhs    = fi_tys fi
    fam_rhs    = fi_rhs fi

    eta_expanded_lhs
      -- eta-expand lhs types, because sometimes data/newtype
      -- instances are eta-reduced; See Trac #9692
      -- See Note [Eta reduction for data family axioms] in TcInstDcls in GHC
      | DataFamilyInst rep_tc <- fam_flavor
      = let (_, rep_tc_args) = splitTyConApp fam_rhs
            etad_tyvars      = dropList rep_tc_args $ tyConTyVars rep_tc
            etad_tys         = mkTyVarTys etad_tyvars
            eta_exp_lhs      = fam_lhs `chkAppend` etad_tys
        in eta_exp_lhs
      | otherwise
      = fam_lhs

    ts = filterOutInvisibleTypes fam_tc eta_expanded_lhs
    synifyTypes = map (synifyType WithinType [])
    ts' = synifyTypes ts
    annot_ts = zipWith3 annotHsType is_poly_tvs ts ts'
    is_poly_tvs = mkIsPolyTvs (tyConVisibleTyVars fam_tc)

{-
Note [Invariant: Never expand type synonyms]

In haddock, we never want to expand a type synonym that may be presented to the
user, as we want to keep the link to the abstraction captured in the synonym.

All code in Haddock.Convert must make sure that this invariant holds.

See https://github.com/haskell/haddock/issues/879 for a bug where this
invariant didn't hold.
-}

-- | A version of 'TcType.tcSplitSigmaTy' that preserves type synonyms.
--
-- See Note [Invariant: Never expand type synonyms]
tcSplitSigmaTyPreserveSynonyms :: Type -> ([TyVar], ThetaType, Type)
tcSplitSigmaTyPreserveSynonyms ty =
    case tcSplitForAllTysPreserveSynonyms ty of
      (tvs, rho) -> case tcSplitPhiTyPreserveSynonyms rho of
        (theta, tau) -> (tvs, theta, tau)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitForAllTysPreserveSynonyms :: Type -> ([TyVar], Type)
tcSplitForAllTysPreserveSynonyms ty = split ty ty []
  where
    split _       (ForAllTy (Bndr tv _) ty') tvs = split ty' ty' (tv:tvs)
    split orig_ty _                          tvs = (reverse tvs, orig_ty)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitPhiTyPreserveSynonyms :: Type -> (ThetaType, Type)
tcSplitPhiTyPreserveSynonyms ty0 = split ty0 []
  where
    split ty ts
      = case tcSplitPredFunTyPreserveSynonyms_maybe ty of
          Just (pred_, ty') -> split ty' (pred_:ts)
          Nothing           -> (reverse ts, ty)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitPredFunTyPreserveSynonyms_maybe :: Type -> Maybe (PredType, Type)
tcSplitPredFunTyPreserveSynonyms_maybe (FunTy arg res)
  | isPredTy arg = Just (arg, res)
tcSplitPredFunTyPreserveSynonyms_maybe _
  = Nothing
