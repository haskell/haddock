{-# LANGUAGE RankNTypes #-}

module Haddock.Backends.Hyperlinker.Ast
    ( enrich
    , RichToken(..), RichTokenType(..), TokenDetails(..)
    ) where

import Haddock.Backends.Hyperlinker.Parser

import qualified GHC

import Control.Applicative
import Data.Data
import Data.Maybe

data RichToken = RichToken
    { rtkToken :: Token
    , rtkDetails :: Maybe TokenDetails
    }

data TokenDetails = TokenDetails
    { rtkType :: RichTokenType
    , rtkName :: GHC.Name
    }

data RichTokenType
    = RtkVar
    | RtkType

enrich :: GHC.RenamedSource -> [Token] -> [RichToken]
enrich src =
    map $ \token -> RichToken
        { rtkToken = token
        , rtkDetails = lookupBySpan (tkSpan token) detailsMap
        }
  where
    detailsMap = variables src ++ types src

type DetailsMap = [(GHC.SrcSpan, TokenDetails)]

lookupBySpan :: Span -> DetailsMap -> Maybe TokenDetails
lookupBySpan tspan = listToMaybe . map snd . filter (matches tspan . fst)

everything :: (r -> r -> r) -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

variables :: GHC.RenamedSource -> DetailsMap
variables =
    everything (<|>) var
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar name))) ->
            pure (sspan, TokenDetails RtkVar name)
        _ -> empty

types :: GHC.RenamedSource -> DetailsMap
types =
    everything (<|>) ty
  where
    ty term = case cast term of
        (Just (GHC.L sspan (GHC.HsTyVar name))) ->
            pure (sspan, TokenDetails RtkType name)
        _ -> empty

matches :: Span -> GHC.SrcSpan -> Bool
matches tspan (GHC.RealSrcSpan aspan)
    | rs && cs && re && ce = True
  where
    rs = (posRow . spStart) tspan == GHC.srcSpanStartLine aspan
    cs = (posCol . spStart) tspan == GHC.srcSpanStartCol aspan
    re = (posRow . spEnd) tspan == GHC.srcSpanEndLine aspan
    ce = (posCol . spEnd) tspan == GHC.srcSpanEndCol aspan
matches _ _ = False
