{-# LANGUAGE TypeFamilies #-}

module Control.Lens.Monadic.Classes where

import Control.Applicative
import Control.Arrow
import Control.Category.Unicode
import Control.Lens (Lens)
import qualified Control.Lens as L
import Control.Monad
import Control.Monad.Classes (MonadState, MonadReader, MonadWriter, MonadLocal)
import qualified Control.Monad.Classes as M
import Data.Functor.Identity

-- state

gets :: (MonadState α m) => ((a -> Const a b) -> (α -> Const a β)) -> m a
gets = M.gets ∘ L.get

puts :: (MonadState α m) => ((a -> (a, b)) -> (α -> (a, α))) -> b -> m a
puts l = modify l ∘ const

state :: (MonadState α m) => ((a -> (c, b)) -> (α -> (c, α))) -> (a -> (c, b)) -> m c
state l f = M.state (l f)

modify :: (MonadState α m) => ((a -> (a, b)) -> (α -> (a, α))) -> (a -> b) -> m a
modify l f = state l (id &&& f)

modifyM :: (MonadState α m) => Lens α α a b -> (a -> m b) -> m a
modifyM l f = M.get >>= liftA2 (<$) (L.get l) (l f >=> M.put)

-- reader

asks :: (MonadReader α m) => ((a -> Const a b) -> (α -> Const a β)) -> m a
asks l = L.get l <$> M.ask

local :: (MonadLocal α m) => ((a -> Identity b) -> (α -> Identity α)) -> (a -> b) -> m c -> m c
local l = M.local ∘ L.modify l

localM :: (MonadReader α m, MonadLocal α m) => ((a -> m b) -> (α -> m α)) -> (a -> m b) -> m c -> m c
localM l f x = M.ask >>= l f >>= flip M.local x ∘ pure

-- writer

tells :: (MonadWriter β m, Monoid α) => ((a -> Identity b) -> (α -> Identity β)) -> b -> m ()
tells l x = M.tell (L.set l x mempty)
