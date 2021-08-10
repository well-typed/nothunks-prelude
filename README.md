# nothunks-prelude

**THIS LIBRARY IS IN EARLY STAGES OF DEVELOPMENT AND NOT YET SUITABLE FOR USE.**

## Introduction

The `nothunks-prelude` is intended as a batteries-included drop-in replacement
of a number of standard libraries, providing strict variants of many of the
standard data types that are in common use. "Strict" here means that they keep
values in weak-head normal form (WHNF, not necessarily in NF). Put another way,
the library offers you the tools you need to keep your values thunk-free.

Design goals for the library include:

* Coverage of all commonly used types in a uniform manner: one libary to cover
  most needs. This includes data types (such as `Maybe`, `[]`, or `Map`), monads
  (such as `StateT`), and references (such as `IORef`); see full list below.

  Uniformity is improved by the use of a (kind-polymorphic) type family called
  `Strict`, associating a strict type with each lazy type. Examples:

  ```haskell
  Strict [] a
  Strict (,) a b
  Strict IORef a
  Strict StateT s m a
  ```

* For each type, full coverage of the common API for that type: to use the
  strict version of a type, it should suffice to just change an import.

  For example:

  ```haskell
  Control.Concurrent.MVar.putMVar ::        MVar a -> a -> IO ()
  NoThunks.MVar.putMVar           :: Strict MVar a -> a -> IO ()
  ```

* O(1) conversion from the "strict" version to the lazy version, providing
  efficient interoperability with code that expects the original lazy version.
  However, the strict version _must_ be a distinct type from the lazy one,
  so that class instances (such as `Functor`) can be defined correctly.

* Support for `nothunks`, enabling testing for unexpected thunks. There are
  two aspects to this:

  - `NoThunks` class instances for all datatypes defined in `nothunks-prelude`.

  - For functions that are commonly used to store long-lived application state,
    the library offers variants that _check_ that that state is thunk-free. The
    signature of these functions is more restrictive than their free-spirited
    counter-parts; for example:

    ```haskell
    NoThunks.MVar.Checked.putMVar :: NoThunks a => Strict MVar a -> a -> IO ()
    ```

    Checking for thunks at runtime is a potentially expensive operation. You
    should therefore use the checked form of the function only in testing. (Both
    variants will guarantee that the value is in WHNF; the checked form
    _additionally_ verifies that this implies that there are no unexpected
    thunks at all in the value.)

    (TODO: Should we make it easier to switch between these two variants? How?
    A cabal flag that enables/disables the runtime check could work, but such a
    flag should probably not affect the _signature_ of the function: if that
    flag adds the `NoThunks ..` => constraint, then enabling the flag might not
    just cause compilation failures in the library or application under
    development, but also in any of its dependencies if those dependencies
    happen to use `nothunks-prelude` themselves.)

* (Internal design goal:) Implement the above with a minimum of effort, using
  code generation (TH) where possible, and a maximum of reliability (extensive
  test suite that verifies both that the lazy and the strict versions are
  equivalent if we ignore thunks, and that the strict versions are indeed
  strict).

## Supported libraries

* `base`

  Common datatypes (which should be strict in the value they store):

  - `Data.Either`
  - `Data.List`
  - `Data.List.NonEmpty`
  - `Data.Maybe`
  - `Data.Tuple`

  References (which should be strict in the value they reference):

  - `Data.IORef`
  - `Data.STRef`
  - `Control.Concurrent.MVar`

* `transformers`

  Common monads

  - `Control.Monad.Trans.State`
    Although `transformers` offers both a lazy and a strict state monad, neither
    of these is actually strict in the state itself.
  - `Control.Monad.Trans.Reader`
  - `Control.Monad.Trans.Writer`
    The strict version of this must be defined either as a restricted state
    monad or else in CPS form in order to be properly strict in the output.

* `stm`

  References

  - `Control.Concurrent.STM.TVar`

  (TODO: Possibly also some of the other STM types, such as channels etc.)

* `containers`

  Common datatypes

  - `Data.IntMap`
  - `Data.IntSet`
  - `Data.Map`
  - `Data.Sequence`
  - `Data.Set`
  - `Data.Tree`

  Although `containers` already provides strict and lazy variants of these
  types, they are not distinct types; we therefore need to wrap the strict
  variant.

* `unordered-containers`

  Common datatypes

  - `Data.HashMap`

  (`Data.HashSet` is _already_ strict.)

* `vector`

  Common datatypes

  - `Data.Vector` (TODO: consider which variants make sense here)

  TODO: Some other array libraries?

## Design overview

We reify each "API" (such as the list `[]` API, the `MVar` API, the `StrictT`
API, ..) as a record that is parametric in the type we're defining. For example,
`NoThunks.Internal.List.API` defines

```haskell
api :: Q Type -> API
api list = API {
      apiFunctions = [
          ('(++), [t| forall a. $list a -> $list a -> $list a |])
        , ..
        ]
    , apiInstances = [
          do a <- newName "a"
             return ([a], [eq (varT a)], eq (list `appT` varT a))
        , ..
        ]
    , apiOverrides = Set.fromList [
          'map
        , ..
        ]
    }

apiLazy, apiStrict :: API
apiLazy   = api $ [t|        [] |]
apiStrict = api $ [t| Strict [] |]
```

The API definition includes the set of functions that are defined for this type,
the classes that this type is an instance of, and which functions must be
redefined in order for them to be strict. Such a definition can be used in a
number of ways:

* As a sanity check, we can create a module that _duplicates_ the lazy API,
  simply generating top-level definitions for each function and each instance
  using the signatures provided by the API, with an implementation that is
  simply a reference to the corresponding function from the original API.

  ```haskell
  module Test.Sanity.API.List where

  import qualified Data.List as Lazy

  -- Generated by TH:
  map :: (a -> b) -> [a] -> [b]
  map = Lazy.map
  ```

  This is a useful sanity check, because it verifies that the API definition
  does in fact match the signatures of the original type.

* We can create a module that defines the proper _strict_ API (the main goal of
  this library). Any function that doesn't have an override is merely a wrapper
  around the original function from the lazy type:

  ```haskell
  module NoThunks.List where

  import NoThunks.Internal.List.Strict as Strict
  import Data.List as Lazy

  -- Generated by TH:
  reverse :: Strict [] a -> Strict [] a
  reverse = coerce Lazy.reverse
  ```

  (The use of `coerce` is possible because `Strict []` is merely a `newtype`
  around `[]`). For functions that _do_ have overrides, the exported function is
  a wrapper around the function from the module that provides that override:

  ```haskell
  -- Generated by TH:
  map :: (a -> b) -> Strict [] a -> Strict [] b
  map = Strict.map
  ```

  This focus on "which functions need overrides and which don't" is part of
  how we reduce workload: `reverse` is already strict, and so doesn't need to
  be redefined, but `concat` does. We will _discover_ which functions need to
  be refined by testing, see below.

* (For pure functions) we can automaically generate QuickCheck property tests
  that verify that (up to strictness) the lazy and the strict API are
  equivalent:

  ```haskell
  -- Generated by TH:
  prop_map :: (Int -> Int) -> [Int] -> [Int]
  prop_map f xs =
          Lazy.map f xs
      === fromStrictList (Strict.map f (toStrictList xs))
  ```

  Such tests are of course only of interest for functions where we provide
  an override, other functions do not need to be tested.

  TODO: I implemented this testing approach in an earlier sketch of this
  library, but haven't yet ported it to this new version.

  TODO: Discuss how the type was generated (in particular, the choice of `Int`).

* (For pure functions) we can automatically generate QuickCheck property tests
  that verify that the strict functions are indeed strict.

  These tests should be run for _all_ functions, whether or not they have an
  override. If they do not have an override, then we are checking whether the
  _original_ function is already strict enough (such as `reverse`); if they _do_
  have an override, we are checking that the _override_ is strict enough
  (maybe the override was not defined correctly).

  This enables us to _discover_ which functions we need to override: we start
  with no overrides, and then add overrides until this test passes.

  TODO: Revive this from the previous sketch of this library and provide an
  example.

* TODO: Think about how the testing approach scales to effectful APIs
  (specifically, reference cells).

* TODO: Think about how the testing approach scales to the various monad
  transformers.

* TODO: For libraries such as `containers` that already provide a strict and
  a lazy variant, we just need code generation to provide the wrapping. No
  testing necessary.



