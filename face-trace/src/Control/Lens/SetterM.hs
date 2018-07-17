{-# LANGUAGE RankNTypes #-}
module Control.Lens.SetterM where

import Control.Lens


-- | A variant of 'Setter' whose 'over' takes an @a -> m b@ instead of an
-- @a -> b@. This is possible because the optic goes through an @m@, so we can
-- add effects at the end of that computation.
data SetterM m s t a b = SetterM
  { unSetterM :: Setter s t a b
  , overM     :: (a -> m b) -> s -> t
  }

-- | Unlike most optics, 'id' is not a valid 'SetterM', since it doesn't go
-- through an @m@. The closest equivalent is 'monadicSetter', which goes
-- through a single @m@.
monadicSetter :: Monad m
              => SetterM m (m a) (m b) a b
monadicSetter = SetterM
  { unSetterM = mapped
  , overM     = (=<<)
  }

-- | 'SetterM's can be composed with ordinary 'Setter's on the left, and with
-- 'Traversal's on the right. We need to define custom operators for this, we
-- cannot use the '.' trick from lens because 'SetterM' does not have the right
-- shape.
thenSetterM :: Monad m
            => Setter s t u v -> SetterM m u v a b -> SetterM m s t a b
thenSetterM s sM = SetterM
  { unSetterM = s . unSetterM sM
  , overM     = over s . overM sM
  }

setterMThen :: Monad m
            => SetterM m s t u v -> Traversal u v a b -> SetterM m s t a b
setterMThen sM l = SetterM
  { unSetterM = unSetterM sM . l
  , overM     = overM sM . traverseOf l
  }

-- | It is also possible to compose two 'SetterM's together, of course.
setterMThenSetterM :: Monad m
                   => SetterM m s t u v -> SetterM m u v a b -> SetterM m s t a b
setterMThenSetterM sM = thenSetterM (unSetterM sM)
