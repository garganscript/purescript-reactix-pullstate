module Reactix.Pullstate
  ( Store, store, subscribe, update, reaction
  , useStoreState, useStoreState1, useStoreState2
  , useStoreState3, useStoreState4, useStoreState5 ) where

import Prelude (Unit, ($), pure)
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn1, EffectFn2, EffectFn3, EffectFn4
  , mkEffectFn1, mkEffectFn3, mkEffectFn4
  , runEffectFn2, runEffectFn3 )
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import FFI.Simple (args2, args3, args4, args5)
import Reactix.React (Hooks)
import Reactix.Utils (hook)

-- store

foreign import data Store :: Type -> Type

-- | Create a new Store
store :: forall s. s -> Store s
store = _store

foreign import _store :: forall s. s -> Store s

-- | Call back the provided effect function when a value updates
-- | Args:
-- |  - store
-- |  - value selector
-- |  - callback: new -> old -> allState -> ()
-- |    - new: new value of state
-- |    - old: old value of state
-- |    - allState: the whole state of the store
subscribe :: forall s t. Store s -> (s -> t) -> (t -> t -> s -> Effect Unit) -> Effect (Effect Unit)
subscribe s f l = runEffectFn3 _subscribe s f (mkEffectFn3 l')
  where l' new all old = l new old all

foreign import _subscribe :: forall s t. EffectFn3 (Store s) (s -> t) (EffectFn3 t s t Unit) (Effect Unit)

-- | Update the value in the store through an effect function which mutates its argument
update :: forall s. Store s -> (s -> Effect Unit) -> Effect Unit
update s u = runEffectFn2 _update s (mkEffectFn1 u)

foreign import _update :: forall s. EffectFn2 (Store s) (EffectFn1 s Unit) Unit

-- | Call back the provided effect function when a value updates
-- | Args:
-- |  - store
-- |  - value selector
-- |  - callback: new -> old -> draft -> allState -> ()
-- |    - new: new value of state
-- |    - old: old value of state
-- |    - draft: new value of state, mutable but slower to read than new
-- |    - allState: the whole state of the store
reaction :: forall s t. Store s -> (s -> t) -> (t -> t -> t -> s -> Effect Unit) -> Effect Unit
reaction s f l = runEffectFn3 _reaction s f (mkEffectFn4 l')
  where l' new draft all old = l new old draft all

foreign import _reaction :: forall s t. EffectFn3 (Store s) (s -> t) (EffectFn4 t t s t Unit) Unit

-- useStoreState

-- | A store state hook. Takes a store and an accessor function.
useStoreState :: forall s t. Store s -> (Store s -> t) -> Hooks t
useStoreState s f = hook $ \_ -> pure $ runFn2 _useStoreState s f

-- | Like useStoreState, but with a memo value
useStoreState1 :: forall s t a. Store s -> (Store s -> t) -> a -> Hooks t
useStoreState1 s f a = unsafeUseStoreState s f [a]

-- | Like useStoreState, but with 2 memo values
useStoreState2 :: forall s t a b. Store s -> (Store s -> t) -> a -> b -> Hooks t
useStoreState2 s f a b = unsafeUseStoreState s f $ args2 a b

-- | Like useStoreState, but with 3 memo values
useStoreState3 :: forall s t a b c. Store s -> (Store s -> t) -> a -> b -> c -> Hooks t
useStoreState3 s f a b c = unsafeUseStoreState s f $ args3 a b c

-- | Like useStoreState, but with 4 memo values
useStoreState4 :: forall s t a b c d. Store s -> (Store s -> t) -> a -> b -> c -> d -> Hooks t
useStoreState4 s f a b c d = unsafeUseStoreState s f $ args4 a b c d

-- | Like useStoreState, but with 5 memo values
useStoreState5 :: forall s t a b c d e. Store s -> (Store s -> t) -> a -> b -> c -> d -> e -> Hooks t
useStoreState5 s f a b c d e = unsafeUseStoreState s f $ args5 a b c d e

-- -- | Call useStoreState passing an array-like object (arguments is
-- -- | acceptable) for memo values without any help from the type system
-- -- | to guarantee the memo value is in fact array-like.
unsafeUseStoreState :: forall s t m. Store s -> (Store s -> t) -> m -> Hooks t
unsafeUseStoreState st fn m = hook $ \_ -> pure $ runFn3 _useStoreStateN st fn m

foreign import _useStoreState :: forall d e. Fn2 (Store d) (Store d -> e) e
foreign import _useStoreStateN :: forall d e m. Fn3 (Store d) (Store d -> e) m e
