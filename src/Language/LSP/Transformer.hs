{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.LSP.Transformer where

import Data.Kind
import Data.Text
import Language.LSP.Types


class Transformer a where
  type Params a
  project :: Params a -> [Text] -> ([Text], a)
  handleDiff :: Params a -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> a -> ([Text], [Text], [TextDocumentContentChangeEvent], a)
  transformPosition :: Params a -> a -> Position -> Maybe Position
  untransformPosition :: Params a -> a -> Position -> Position
  -- untransformPosition :: Params a -> a -> Position -> Maybe Position

data (a :: Type) :> (b :: Type) = a :> b
  deriving Show
infixr :>

instance (Transformer a, Transformer b) => Transformer (a :> b) where
  type Params (a :> b) = Params a :> Params b
  project (xParams :> yParams) lines = (lines'', x :> y)
    where
      (lines', x) = project xParams lines
      (lines'', y) = project yParams lines'
  handleDiff (xParams :> yParams) before after change (x :> y) = (before'', after'', change'', x' :> y')
    where
      (before', after', change', x') = handleDiff xParams before after change x
      (before'', after'', change'', y') = handleDiff yParams before' after' change' y
  transformPosition (xParams :> yParams) (x :> y) p = transformPosition xParams x p >>= transformPosition yParams y
  untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition xParams x (untransformPosition yParams y p)
  -- untransformPosition (xParams :> yParams) (x :> y) p = untransformPosition yParams y p >>= untransformPosition xParams x

data SomeTransformer where
  SomeTransformer :: forall a. (Transformer a) => a -> Params a -> SomeTransformer
