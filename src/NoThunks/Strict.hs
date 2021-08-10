module NoThunks.Strict (
    Strict
  ) where

-- | The strict version of the specified type
--
-- This is kind polymorphic. Some examples:
--
-- * At kind @Type -> Type@:
--
--   > toStrictList   :: forall a. [a] -> Strict [] a
--   > fromStrictList :: forall a. Strict [] a -> [a]
--
-- * At kind @Type -> Type -> Type@:
--
--   > toStrictMap   :: forall k v. Map k v -> Strict Map k v
--   > fromStrictMap :: forall k v. Strict Map k v -> Map k v
--
-- * At kind @Type -> Type@, with monadic @to@ function:
--
--   > toStrictIORef   :: forall a. IORef a -> IO (Strict IORef a)
--   > fromStrictIORef :: forall a. Strict IORef a -> IORef a
--
-- Since these translation functions have different types, we do not attempt
-- to capture this in a class; instead, each type will define its own @to@
-- and @from@ function. However, users should be guaranteed that the @from@
-- function is @O(1)@.
--
-- NOTE: An alternative design would keep 'Strict' at kind @Type@ only, and
-- define instances such as
--
-- > type instance Strict [a] = ..
--
-- instead of
--
-- > type instance Strict [] a = ..
--
-- However, while such a design would make it easier to define the conversion
-- functions once and for all, it would make it impossible to define type
-- class instances for @Strict [a]@ for classes over types at higher kinds
-- (such as 'Foldable').
data family Strict (a :: k) :: k
