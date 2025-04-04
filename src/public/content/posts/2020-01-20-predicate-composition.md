They say functional programming has many essences and the composition is one of them. Thanks to the wonderful dot operator, we know how to compose functions like `a -> b` and `b -> c` to get a function `a -> c`. But in some cases functions are not that simple and it becomes tricky to compose them nicely.

``` haskell
valid :: a -> Bool
valid = \a -> check1 a && (check2 a || check3 a)
  where check1 = undefined :: a -> Bool
        check2 = undefined :: a -> Bool
        check3 = undefined :: a -> Bool
```

It would be lovely to express it in a more declarative way by abstracting away function application and result combination.

``` haskell
valid :: a -> Bool
valid = check1 .&& (check2 .|| check3)

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
```

Apart from implementing combinators for predicate composition, we want to avoid any runtime penalty from using abstractions. In this article we are going to implement the following functions and investigate how far we can go with abstractions until performance degrades. Or maybe it won't degrade. Who knows?

<!--more-->

# Inline implementation

Before we jump into the void, lets draw a baseline by keeping implementation simple.

``` haskell
module Main (main) where

main :: IO ()
main = do
  input <- read <$> getLine
  let result = input < 10 && (input > 0 || even input)
  print result
```

Haskell language is abstract and high-level thus in some cases in order to really understand what the program does we need to look at the intermediate language called [Core](https://www.aosabook.org/en/ghc.html) (or System FC) produced by Glasgow Haskell Compiler (GHC) when it compiles[^1] our program. This reduced code is the end result of GHC's optmisations-by-transformation process, which iteratively rewrites the original code into more optimised versions in a smaller language.

In order to dump the intermediate code in Core language we need to ask GHC to do it. We use `-O` (or `-O2`) to enable optimisations and `-ddump-simpl` to dump the simplified output, which can be combined with `-ddump-to-file` to write result into a file instead of `stdout`. More options are described in the [GHC manual](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/debugging.html#debugging-the-compiler).

``` bash
$ ghc -O -ddump-simpl Main.hs
```

Or if you are using [stack](https://docs.haskellstack.org/en/stable/README/):

``` bash
$ stack ghc -- -O -ddump-simpl Main.hs
```

This [prints a lot of stuff](https://github.com/d12frosted/d12frosted.io/blob/master/assets/snippets/predicate-composition/inline.dump-simpl), so let me focus on the most important part.

``` haskell
...

Main.$seven1 :: Integer
Main.$seven1 = 0

...

Main.$seven2 :: Integer
Main.$seven2 = 2

...

Main.main3 :: Integer
Main.main3 = 10

...

  -- check if input < 10
  case integer-gmp-1.0.2.0:GHC.Integer.Type.ltInteger#
         x1_a6kH    -- input
         Main.main3 -- 10
  of {
    -- false
    __DEFAULT -> GHC.Show.$fShowBool4; -- false

    -- true
    1# ->
      -- check if input > 0
      case integer-gmp-1.0.2.0:GHC.Integer.Type.gtInteger#
             x1_a6kH      -- input
             Main.$seven1 -- 0
      of {
        __DEFAULT ->
          -- check if input is even (e.g. rem input 2 == 0)
          case integer-gmp-1.0.2.0:GHC.Integer.Type.eqInteger#
                 (integer-gmp-1.0.2.0:GHC.Integer.Type.remInteger
                    x1_a6kH      -- input
                    Main.$seven2 -- 2
                 )
                 Main.$seven1    -- 0
          of {
            __DEFAULT -> GHC.Show.$fShowBool4; -- false
            1# -> GHC.Show.$fShowBool2         -- true
          };
        1# -> GHC.Show.$fShowBool2             -- true
      }
  };

...
```

So this how it looks in Core, verbose but really straightforward.

# Reason to read the line

Strictly speaking there is no need for reading integer from `stdin` in our example. After all, we care only about the predicates. But GHC is pretty aggressive in terms of in-lining and simplifications when optimisations are enabled. With `-O2` there will be even more cross-module optimisation compared to `-O`.

``` haskell
module Main (main) where

main :: IO ()
main = do
  let input  = 5
  let result = input < 10 && (input > 0 || even input)
  print result
```

Compiling this module with `-O` produces [the following Core](https://github.com/d12frosted/d12frosted.io/blob/master/assets/snippets/predicate-composition/no-getline.dump-simpl) (83 lines).

``` haskell
main :: IO ()
[GblId,
 Arity=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 40 60}]
main
  = GHC.IO.Handle.Text.hPutStr'
      GHC.IO.Handle.FD.stdout GHC.Show.$fShowBool2 GHC.Types.True
```

As you can see, it figured out that there is no need to evaluate it in runtime. But in order to compare different implementations of composition operators, we don't want compiler to inline the result.

If you are curious about reductions steps, you can pass `-v` option to `ghc` to be more verbose. When you build with `-v`, compilation of the version with `getLine` is less verbose than without.

# Trivial implementation

Now that we have a solid source of nightmares, let's return to cozy nook. Our first step is to create operators in the most trivial manner.

``` haskell
module Main (main) where

main :: IO ()
main = do
  input <- read <$> getLine
  let result = (< 10) .&& ((> 0) .|| even) $ input
  print result

infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \a -> p1 a && p2 a

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .|| p2 = \a -> p1 a || p2 a
```

If we compile it, the relevant part in the Core language is the same.

``` haskell
...

  case integer-gmp-1.0.2.0:GHC.Integer.Type.ltInteger#
         x1_a6m7 Main.main3
  of {
    __DEFAULT -> GHC.Show.$fShowBool4;
    1# ->
      case integer-gmp-1.0.2.0:GHC.Integer.Type.gtInteger#
             x1_a6m7 Main.$seven1
      of {
        __DEFAULT ->
          case integer-gmp-1.0.2.0:GHC.Integer.Type.eqInteger#
                 (integer-gmp-1.0.2.0:GHC.Integer.Type.remInteger
                    x1_a6m7 Main.$seven2)
                 Main.$seven1
          of {
            __DEFAULT -> GHC.Show.$fShowBool4;
            1# -> GHC.Show.$fShowBool2
          };
        1# -> GHC.Show.$fShowBool2
      }
  };

...
```

While our code looks better, there are no runtime penalties. In short, with `-O` option GHC always tries to inline small functions (based on [unfolding-creation-threshold](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using-optimisation.html#ghc-flag--funfolding-creation-threshold=%E2%9F%A8n%E2%9F%A9) and heuristics) thus avoiding the call overhead and enabling other optimisations (like replacing whole expression with its result). And when unfolding doesn't happen for some of the reasons and you really think that it should happen (make such decision based on CPU and memory profiling), then put [INLINE pragma](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#inline-pragma).

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \a -> p1 a && p2 a
{-# INLINE (.&&) #-}
```

Please note that in-lining usually leads to bigger executable.

# Using `newtype` wrappers

If we look at the definition of `.&&` and `.||` we see that they are pretty much the same. The only difference is the use of `&&` instead of `||`.

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \a -> p1 a && p2 a

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .|| p2 = \a -> p1 a || p2 a
```

Maybe there is some magic function that takes a function for combining two booleans, two predicates, a value and returns a boolean? So we can express our combinators with it.

``` haskell
magic :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
magic plus p1 p2 = \a -> p1 a `plus` p2 a
```

Or even more generic one:

``` haskell
gmagic :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
gmagic plus p1 p2 = \a -> p1 a `plus` p2 a
```

This all reminds me of `Semigroup`.

``` haskell
class Semigroup a where
  (<>) :: a -> a -> a

gmagic :: (Semigroup b) => (a -> b) -> (a -> b) -> a -> b
gmagic f g = \a -> f a <> g a
```

Thanks to `Semigroup` the `plus` function is not passed explicitly and `gmagic` become lighter. Now, functions which return type is an instance of `Semigroup` also form `Semigroup` and it's implementation looks familiar.

``` haskell
instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a
```

So it turns out that our `gmagic` function is a binary operator from `Semigroup`. How convenient, isn't it? If we add more parenthesis to the signature you'll notice that it actually takes two functions and produces new one (exactly what we are doing with predicates).

``` haskell
gmagic :: (Semigroup b) => (a -> b) -> (a -> b) -> (a -> b)
gmagic f g = \a -> f a <> g a
```

In Haskell every single data type can have not more than one instance of a given type class. But for some data types there are more than one valid (lawful) instances of a given type class. For example, we know that the set of natural numbers forms different semigroups with different operations: $ ( \mathbb{N}, + ) $ or $ ( \mathbb{N}, \cdot ) $. The same story with booleans - $ ( \mathbb{B}, \wedge ) $ and $ ( \mathbb{B}, \vee ) $ are both valid semigroups.

Restriction for amount of instances means that we need to wrap our data types when we need to create multiple instances. A wrapper per each instance. That leads to an awful runtime cost - wrapping and unwrapping are not free. That's why we use `newtype` to create wrappers. In compile time the `newtype` wrapper is not equal to the type that is being wrapped, so we can use different instances. But since the types are isomorphic, all the wrapping and unwrapping can be removed by compiler, so we don't have any runtime costs anymore.

When it comes to booleans with conjunction (`&&`) or disjunction (`||`), we don't need to define our own wrappers since `Data.Monoid` already provides them - `All` and `Any`.

``` haskell
> getAll (All True <> All False)
False

> getAny (Any True <> Any False)
True
```

We can fetch it all together and get new definition of `.&&` and `.||`.

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = getAll . (All . p1 <> All . p2)

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .|| p2 = getAny . (Any . p1 <> Any . p2)
```

I've heard multiple times that `newtype` is erased during compilation and by inspecting [the dumped Core](https://github.com/d12frosted/d12frosted.io/blob/master/assets/snippets/predicate-composition/operator-newtype.dump-simpl) we can confirm that this version is not different from the previous one.

However we didn't improve the code. I'd say that we degraded. While we abstracted away function application, we have strengthened the link between the definition shape and the binary operation, which now appears three times on two different levels. Not good, definitely not good.

# Coercion

What comes to the rescue is coercion. Starting with GHC 7.8 there is a new type class allowing conversion between any two types that are representationally equal.

``` haskell
-- Data.Coerce
class Coercible a b where
  coerce :: a -> b
```

But what does it mean to be representationally equal? And are there any other types[^2] of type equality? It turns out that there are two of them and they were [introduced](https://gitlab.haskell.org/ghc/ghc/wikis/roles) as a solution for a long existing hole in a type system.

Nominal equality means that types are *really* equal. If two types have the same name (expanding synonyms) they are nominally equal. If they don't have the same name, well, then they are not nominally equal.

But what about `newtype` wrappers like `All` and `Any`? We know that they are isomorphic to `Bool` (and mutually as well). Are they equal? Here comes the second kind of type equality – representational. They all share the same representation. While `All` and `Bool` are representationally equal, they are not equal nominally!

So all that means that we can use `coerce` to convert from `All` to `Bool` and back. Let's try it.

``` haskell
> :m +Data.Coerce
> :m +Data.Monoid

> :t coerce
coerce :: Coercible a b => a -> b

> :t getAll . coerce
getAll . coerce :: Coercible a Bool => a -> Bool
```

Wow, this is kind of tricky. The `getAll . coerce` expression literally says – 'give me something representationally equal to `Bool` and I will get to back the `Bool`'. It will do all the conversion for us.

When we look at the previous implementation of `.&&` we might notice that we actually ~~convert~~ coerce `Bool` to `All` and then get back the `Bool` value.

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = getAll . (All . p1 <> All . p2)
```

Maybe we can replace `All` with `coerce`?

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = getAll . (coerce . p1 <> coerce . p2)
```

And it works. We can repeat the trick with `.||`, but at this point we can move this patter to a helper operator `<~>`.

``` haskell
f <~> g = coerce . f  <> coerce . g
-- or in other words
f <~> g = \a -> coerce (f a) <> coerce (g a)

infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = getAll . (p1 <~> p2)
```

I specially omitted the type signature of `<~>`. It's not our job to infer the types, but let's steal some bread from GHC's table.

We know that the type of `f` should be `a -> b`. Previously we put a constraint on `b` to form `Semigroup`. But now we `coerce` it some type and only then use `<>`. The result of `coerce (f a)` must form `Semigroup`. Which means that if `f` has type `a -> b` then we need be able to covert `b` to some type `c` which is semigroup.

``` haskell
(<~>) :: (Coercible b c, Monoid c) => (a -> b) -> (a -> b) -> a -> c
f <~> g = coerce . f <> coerce . g
```

And you know what? It works! But if you think about the `g`, then you realise that `f a` and `g a` are independent, the only requirement is to be able to coerce them to the same type `c` that forms `Semigroup`.

``` haskell
module Main (main) where

import           Data.Coerce
import           Data.Monoid

main :: IO ()
main = do
  input <- read <$> getLine
  let result = (< 10) .&& ((> 0) .|| even) $ input
  print result

(<~>) :: ( Coercible b1 c
         , Coercible b2 c
         , Monoid c
         )
      => (a -> b1) -> (a -> b2) -> a -> c
f <~> g = coerce . f <> coerce . g

infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = getAll . (p1 <~> p2)

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .|| p2 = getAny . (p1 <~> p2)
```

This works, this composes. You can also use it with other semigroups like `Sum` and `Product`. But it might look a little bit weird.

``` haskell
> getSum . ((*2) <~> (+100)) $ 15
145
```

So instead, let's look at [the Core dump](https://github.com/d12frosted/d12frosted.io/blob/master/assets/snippets/predicate-composition/operator-coerce.dump-simpl).

``` haskell
...

case ds2_a6m8 of {
  [] ->
    case integer-gmp-1.0.2.0:GHC.Integer.Type.ltInteger#
           x1_a6m7 Main.main3
    of {
      __DEFAULT -> GHC.Show.$fShowBool4;
      1# ->
        case integer-gmp-1.0.2.0:GHC.Integer.Type.gtInteger#
               x1_a6m7 Main.$seven1
        of {
          __DEFAULT ->
            case integer-gmp-1.0.2.0:GHC.Integer.Type.eqInteger#
                   (integer-gmp-1.0.2.0:GHC.Integer.Type.remInteger
                      x1_a6m7 Main.$seven2)
                   Main.$seven1
            of {
              __DEFAULT -> GHC.Show.$fShowBool4;
              1# -> GHC.Show.$fShowBool2
            };
          1# -> GHC.Show.$fShowBool2
        }
    };

...
```

The important bits are the same.

# Criterion

I bet that at this point it's obvious, but they perform similarly – the naive implementation and the most abstract one with coercion and `newtype` wrappers. We know this because we inspected the dumped Core, but we can also refer to criterion to inspect the runtime performance.

``` example
benchmarking single/naive
time                 3.027 ns   (3.011 ns .. 3.043 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.017 ns   (3.009 ns .. 3.029 ns)
std dev              31.73 ps   (22.17 ps .. 48.50 ps)
variance introduced by outliers: 12% (moderately inflated)

benchmarking single/coerce
time                 3.017 ns   (3.009 ns .. 3.025 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.026 ns   (3.015 ns .. 3.055 ns)
std dev              56.91 ps   (26.62 ps .. 114.5 ps)
variance introduced by outliers: 30% (moderately inflated)
```

<div class="criterion" file="predicate-composition/single.json" type="horizontalBar" height="120" xAxisBeginAtZero="true">

</div>

# Final words

I love that in Haskell one can use *some* of the abstractions without hurting the runtime. After all, as developers we want to simplify our *development* life with minimal negative influence on the application.

Today we implemented two simple operators for predicate composition using semigroups and coercion. And we saw that they don't introduce runtime penalty. Techniques that made it possible are usable in other scenarios.

``` haskell
module Data.Monoid.Extra
  ( (.&&)
  , (.||)
  ) where

import           Data.Coerce
import           Data.Monoid

infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = getAll . (p1 <~> p2)

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .|| p2 = getAny . (p1 <~> p2)

(<~>) :: ( Coercible b1 c
         , Coercible b2 c
         , Monoid c
         )
      => (a -> b1) -> (a -> b2) -> a -> c
f <~> g = coerce . f <> coerce . g
```

# Evolution

I love the [The Evolution of a Haskell Programmer](https://willamette.edu/~fruehr/haskell/evolution.html) by Fritz Ruehr. And to keep the traction of this evolution path, we should step back and reflect on atrocious results. We all love functions, don't we? And functions are known functors and applicatives. So instead of going this lengthy path, we could just do something dead simple.

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = (&&) <$> p1 <*> p2

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .|| p2 = (||) <$> p1 <*> p2
```

Or using `liftA2`:

``` haskell
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) p2 = liftA2 (&&)

infixr 2 .||
(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) = liftA2 (||)
```

Stay safe!

# References

- [The Glasgow Haskell Compiler](https://www.aosabook.org/en/ghc.html) by [Simon Marlow](https://www.aosabook.org/en/intro2.html#marlow-simon) and [Simon Peyton-Jones](https://www.aosabook.org/en/intro2.html#peyton-jones-simon).
- Real World Haskell Chapter 25. Profiling and optimization by Bryan O'Sullivan, Don Stewart, and John Goerzen.
- [Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/debugging.html#id2) Debugging the compiler.
- [Roles](https://gitlab.haskell.org/ghc/ghc/wikis/roles) on GHC Wiki.

[^1]: Aaah, the tautology…

[^2]: Kinds?
