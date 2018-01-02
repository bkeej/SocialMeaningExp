{-# LANGUAGE DeriveFunctor #-}

module Utils (
  Mass(Mass, getFstMass, getSndMass),
  MassT(MassT, runMassT),
  Sum(Sum, getSum),
) where

import           Control.Monad             (ap)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Monoid
import           Data.Semiring

data Mass w a = Mass { getFstMass :: w, getSndMass :: a }
  deriving (Functor, Show)

instance Semiring w => Monad (Mass w) where
  return x = Mass one x
  m >>= f  = Mass (w1 <.> w2) x
    where (Mass w1 (Mass w2 x)) = fmap f m

instance Semiring w => Applicative (Mass w) where
  pure  = return
  (<*>) = ap

newtype MassT w m a = MassT { runMassT ::  m (Mass w a) }
  deriving Functor

instance (Semiring w, Monad m) => Monad (MassT w m) where
  return   = MassT . return . return
  mx >>= f = MassT $ do
    Mass w1 x1 <- runMassT mx
    Mass w2 x2 <- runMassT (f x1)
    return (Mass (w1 <.> w2) x2)

instance Semiring w => MonadTrans (MassT w) where
  lift = MassT . fmap return

instance (Semiring w, Monad m) => Applicative (MassT w m) where
  pure  = return
  (<*>) = ap

instance Num a => Semiring (Sum a) where
  one   = Sum 1
  (<.>) = (*)

instance Fractional a => Fractional (Sum a)
  where fromRational  = Sum . fromRational
        (/) = (<*>) . ((/) <$>)
