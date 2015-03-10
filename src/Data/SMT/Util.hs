{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language Rank2Types #-}
{-# Language PolyKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language DeriveDataTypeable #-}
module Data.SMT.Util where

import Control.Failure
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 
import Data.Typeable
import Data.Maybe
import Control.Applicative
import Data.SMT.Abstract.Types
import Data.Extensible

widening :: forall a (h :: k -> *) (xs :: [k]) . (forall (x :: k) . h x -> a) -> h :| xs -> a
widening f (UnionAt pos x) = f x
{-# INLINE widening #-}

class Substitute e where
  subst :: IntMap Int -> e -> e

instance Substitute term => Substitute (AbstFormula term cs :| cs) where
  subst m = widening (subst' m)
    where
      subst' :: forall term (c :: FComponent) cs . Substitute term =>
                IntMap Int -> AbstFormula term cs c -> AbstFormula term cs :| cs
      subst' m (t1 :=: t2) = embed $ subst m t1 :=: subst m t2
      subst' m (t1 :<: t2) = embed $ subst m t1 :<: subst m t2
      subst' m (f1 :&: f2) = embed $ subst m f1 :&: subst m f2
      subst' m (f1 :|: f2) = embed $ subst m f1 :|: subst m f2

instance (INT ∈ cs) => Substitute (AbstTerm cs :| cs) where
  subst m = widening (subst' m)
    where
      subst' :: forall (c :: TComponent) cs . (INT ∈ cs) =>
                IntMap Int -> AbstTerm cs c -> AbstTerm cs :| cs
      subst' m t@(Var i) = fromMaybe (embed t) (embed <$> (IConst <$> IM.lookup i m))
      subst' m t@(IConst n) = embed t
      subst' m (Neg t) = embed $ Neg (subst m t)
      subst' m (t1 :+: t2) = embed $ subst m t1 :+: subst m t2
      subst' m (t1 :*: t2) = embed $ subst m t1 :*: subst m t2

class Interpretable e v | e -> v where
  eval :: IntMap Int -> e -> v

instance Interpretable term Int => Interpretable (AbstFormula term cs :| cs) Bool where
  eval m = widening (eval' m)
    where
      eval' :: forall (c :: FComponent) cs . Interpretable term Int =>
               IntMap Int -> AbstFormula term cs c -> Bool
      eval' m (t1 :=: t2) = eval m t1 == eval m t2
      eval' m (t1 :<: t2) = eval m t1 < eval m t2
      eval' m (f1 :&: f2) = eval m f1 && eval m f2
      eval' m (f1 :|: f2) = eval m f1 || eval m f2

-- XXX: We should add a constraint: ¬(VAR ∈ cs)
instance Interpretable (AbstTerm cs :| cs) Int where
  eval m = widening (eval' m)
    where
      eval' :: forall (c :: TComponent) cs . IntMap Int -> AbstTerm cs c -> Int
      eval' m t@(Var i) =
        case IM.lookup i m of
          Just v -> v
          Nothing -> error $ "Undefined: " ++ ppr t
      eval' m (IConst n) = n
      eval' m (Neg t) = negate $ eval m t
      eval' m (t1 :+: t2) = eval m t1 + eval m t2
      eval' m (t1 :*: t2) = eval m t1 * eval m t2