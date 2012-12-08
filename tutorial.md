---
layout: page
title: Getting Started
---

First, install `lens`.

```
$ cabal install lens
```

Then, start up ghci

```
$ ghci
GHCi, version 7.4.2: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
```
and import `Control.Lens`.

```haskell
ghci> import Control.Lens
```

Now, you can read from lenses

```haskell
ghci> ("hello","world")^._2
"world"
```

and you can write to lenses.

```haskell
ghci> set _2 42 ("hello","world")
("hello",42)
```

Composing lenses for reading (or writing) goes in the order an imperative programmer would expect, and just uses `(.)` from the `Prelude`.

```haskell
ghci> ("hello",("world","!!!"))^._2._1
"world"
```

```haskell
ghci> set (_2._1) 42 ("hello",("world","!!!"))
("hello",(42,"!!!"))
```

You can make a `Getter` out of a pure functions with `to`.

```haskell
ghci> "hello"^.to length
5
```

A `Getter` works like a `Lens`, except you can only read from it, not write.

You can easily compose a `Getter` with a `Lens` just using `(.)`. No explicit coercion is necessary.

```haskell
ghci> ("hello",("world","!!!"))^._2._2.to length
3
```

As we saw above, you can write to lenses and these writes can change the type of the container. `(.~)` is an infix alias for `set`.

```haskell
ghci> _1 .~ "hello" $ ((),"world")
("hello","world)
```
It can be used in conjunction with `(&)` for a more familiar von Neumann style assignment syntax:

```haskell
ghci> ((), "world") & _1 .~ "hello"
("hello","world)
```

Conversely `view`, can be used as an prefix alias for `(^.)`.

```haskell
ghci> view _2 (10,20)
20
```

Lens comes "Batteries Included" with many lenses for manipulating common data types, such as Maps:

```haskell
ghci> import Data.Map as Map
ghci> Map.fromList [("hello","there")] ^.at "hello"
Just "there"
```

You can insert

```haskell
ghci> Map.fromList [("hello","there")] & at "hello" ?~ "world"
fromList [("hello","world")]
```

and delete with this lens

```haskell
ghci> Map.fromList [("hello","there")] & at "hello" .~ Nothing
fromList []
```

You can let the library automatically derive lenses for fields of your data type

```haskell
{-# LANGUAGE TemplateHaskell #-}
data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo
```

This will automatically generate the following lenses:

```haskell
bar, baz :: Simple Lens (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b
```

A `Lens` takes 4 parameters because it can change the types of the whole when you change the type of the part.

Often you won't need this flexibility, a `Simple Lens` takes 2 parameters, and can be used directly as a `Lens`.

Just like how we can write a `Getter` that can only be used for retrieving information, we can write a `Setter`
which can only be used for updates. Like a using `fmap` on a `Functor` updating a `Setter` can modify multiple
targets!

The canonical example of a setter is 'mapped':

```haskell
mapped :: Functor f => Setter (f a) (f b) a b
```

`over` is then analogous to `fmap`, but parameterized on the Setter.

```haskell
ghci> fmap succ [1,2,3]
[2,3,4]
ghci> over mapped succ [1,2,3]
[2,3,4]
```

The benefit is that you can use any `Lens` as a `Setter`, and the composition of setters with other setters or lenses using `(.)` yields
a `Setter`.

```haskell
ghci> over (mapped._2) succ [(1,2),(3,4)]
[(1,3),(3,5)]
```

`(%~)` is an infix alias for 'over', and the precedence lets you avoid swimming in parentheses:

```haskell
ghci> ([(42, "hello")],"world") & _1.mapped._2.mapped %~ succ
([(42, "ifmmp")],"world")
```

There are a number of combinators that resemble the `+=`, `*=`, etc. operators from C/C++ for working with the monad transformers.

There are `+~`, `*~`, etc. analogues to those combinators that work functionally, returning the modified version of the structure.

```haskell
ghci> (1,2) & both *~ 2
(2,4)
```

There are combinators for manipulating the current state in a state monad as well

```haskell
fresh :: MonadState Int m => m Int
fresh = id <+= 1
```

Anything you know how to do with a `Foldable` container, you can do with a `Fold`

```haskell
ghci> :m + Data.Char Data.Text.Lens
ghci> allOf (folded.text) isLower ["hello"^.packed, "goodbye"^.packed]
True
```

There are actually a large number of variations on the concept of a `Lens` provided by the library, in particular a `Traversal`
generalizes `traverse` from `Data.Traversable`.

You can also use this for generic programming. Combinators are included that are based on Neil Mitchell's `uniplate`, but which
have been generalized to work on or as lenses, folds, and traversals.

```haskell
ghci> :m + Data.Data.Lens
ghci> anyOf biplate (=="world") ("hello",(),[(2::Int,"world")])
True
```

As alluded to above, anything you know how to do with a `Traversable` you can do with a `Traversal`.

```haskell
ghci> mapMOf (traverse._2) (\xs -> length xs <$ putStrLn xs) [(42,"hello"),(56,"world")]
"hello"
"world"
[(42,5),(56,5)]
```

Moreover, many of the lenses supplied are actually isomorphisms, that means you can use them directly as a lens or getter:

```haskell
ghci> let hello = "hello"^.packed
"hello"
ghci> :t hello
hello :: Text
```

but you can also flip them around and use them as a lens the other way with `from`!

```haskell
ghci> hello^.from packed.to length
5
```

You can automatically derive isomorphisms for your own newtypes with `makeIso`. e.g.

```haskell
newtype Neither a b = Neither { _nor :: Either a b } deriving (Show)
makeIso ''Neither
```

will automatically derive

```haskell
neither :: Iso (Neither a b) (Neither c d) (Either a b) (Either c d)
nor :: Iso (Either a b) (Either c d) (Neither a b) (Neither c d)
```

such that

```haskell
from neither = nor
from nor = neither
neither.nor = id
nor.neither = id
```

There is also a fully operational, but simple game of [Pong](https://github.com/ekmett/lens/blob/master/examples/Pong.hs) in the [examples/](https://github.com/ekmett/lens/blob/master/examples/) folder.

There are also a couple of hundred examples distributed throughout the haddock documentation.
