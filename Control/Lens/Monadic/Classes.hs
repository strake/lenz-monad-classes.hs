{-# LANGUAGE TypeFamilies #-}

module Control.Lens.Monadic.Classes where

import Control.Applicative
import Control.Arrow
import Control.Category.Unicode
import Control.Lens as L
import Control.Monad
import Control.Monad.Classes (MonadState, MonadReader, MonadWriter, MonadLocal)
import qualified Control.Monad.Classes as M

-- state

gets :: (MonadState α m) => Lens α β a b -> m a
gets = M.gets ∘ L.get

puts :: (MonadState α m) => Lens α α a b -> b -> m a
puts l = liftA2 pure (gets l) ∘ M.modify ∘ L.set l

state :: (MonadState α m) => Lens α α a b -> (a -> (c, b)) -> m c
state l f = f <$> gets l >>= \ (x, s) -> x <$ puts l s

modify :: (MonadState α m) => Lens α α a b -> (a -> b) -> m a
modify l = liftA2 pure (gets l) ∘ M.modify ∘ L.modify l

modifyM :: (MonadState α m) => Lens α α a b -> (a -> m b) -> m a
modifyM l f = M.get >>= liftA2 (<$) (L.get l) (l f >=> M.put)

-- reader

asks :: (MonadReader α m) => Lens α β a b -> m a
asks = (<$> M.ask) ∘ L.get

local :: (MonadLocal α m) => Lens α α a b -> (a -> b) -> m c -> m c
local = M.local ∘∘ L.modify where f ∘∘ g = (f ∘) ∘ g

localM :: (MonadReader α m, MonadLocal α m) => Lens α α a b -> (a -> m b) -> m c -> m c
localM l f x = M.ask >>= l f >>= flip M.local x ∘ pure

-- writer

tells :: (MonadWriter β m, Monoid α) => Lens α β a b -> b -> m ()
tells l x = M.tell (L.set l x mempty)
