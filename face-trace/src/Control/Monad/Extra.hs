module Control.Monad.Extra ((>>>), (<&>), (>=>), (>&>)) where

import Control.Category ((>>>))
import Control.Lens ((<&>))
import Control.Monad ((>=>))


-- | Use instead of '(>>>)' immediately after a monadic action.
--
-- '(>>>)' composes pure transformations, while '(>=>)' composes monadic
-- transformations. They are both associative, so we can easily extract and name
-- common subsequences. The same applies to a sequence of pure transformations
-- followed by monadic transformations:
--
--     f >>> (g >=> f)
--   = \x -> g (f x) >>= f
--   = (f >>> g) >=> f
--
-- But not to a sequence of monadic transformations followed by pure
-- transformations:
--
--     f >=> (g >>> h)
--   = \x -> h (g x) >>= f
--  /= \x -> h (f x >>= g)
--   = (f >=> g) >>> h
--
-- Another annoyance is that the pure transformations which occur before and
-- after the monadic transformations have a very different meaning: the former
-- apply to the input value, while the later apply to the monadic computation,
-- not the output value.
--
-- Using '(>&>)' solves both problems: it composes a monadic transformation with
-- a pure transformation by applying the pure transformation to the output
-- value, and it associates with '(>=>)':
--
--     f >=> (g >&> h)
--   = \x -> f x >>= (\y -> fmap h (g y))
--   = \x -> fmap h (f x >>= g)
--   = (f >=> g) >&> h
(>&>) :: Functor f
      => (a -> f b)
      -> (b -> c)
      -> (a -> f c)
f >&> g = f >>> fmap g

infixr 1 >&>
