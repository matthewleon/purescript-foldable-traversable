module Data.Dissectable where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Rec.Class
import Data.Bifunctor
import Data.Bifunctor.Clown
import Data.Bifunctor.Joker
import Data.Bifunctor.Product
import Data.Either
import Data.List
import Data.Traversable
import Data.Tuple

class (Traversable f, Bifunctor d) <= Dissectable f d | f -> d where
  right :: forall c j. Either (f j) (Tuple (d c j) c) -> Either (f c) (Tuple (d c j) j)
  
mapDefault :: forall f d a b. Dissectable f d => (a -> b) -> f a -> f b
mapDefault f xs = tailRec step (Left xs) where
  step = right >>> case _ of
           Left ys -> Done ys
           Right (Tuple dba a) -> Loop (Right (Tuple dba (f a)))
      
traverseP :: forall m f d a b. Dissectable f d => MonadRec m => (a -> m b) -> f a -> m (f b)
traverseP f xs = tailRecM step (Left xs) where
  step = right >>> case _ of
           Left ys -> pure (Done ys)
           Right (Tuple dba a) -> Loop <<< Right <<< Tuple dba <$> f a
