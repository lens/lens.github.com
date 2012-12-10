---
layout: tutorial
title: Getting Started
---

### Getting Started

First, install `lens`.

    $ cabal install lens

Then, start up ghci

    $ ghci
    GHCi, version 7.4.2: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.

and import `Control.Lens`.

{% highlight haskell %}
ghci> import Control.Lens
{% endhighlight %}

Now, you can read from lenses

{% highlight haskell %}
ghci> ("hello","world")^._2
"world"
{% endhighlight %}

and you can write to lenses.

{% highlight haskell %}
ghci> set _2 42 ("hello","world")
("hello",42)
{% endhighlight %}

Composing lenses for reading (or writing) goes in the order an imperative programmer would expect, and just uses `(.)` from the `Prelude`.

{% highlight haskell %}
ghci> ("hello",("world","!!!"))^._2._1
"world"
{% endhighlight %}

{% highlight haskell %}
ghci> set (_2._1) 42 ("hello",("world","!!!"))
("hello",(42,"!!!"))
{% endhighlight %}

You can make a `Getter` out of a pure functions with `to`.

{% highlight haskell %}
ghci> "hello"^.to length
5
{% endhighlight %}

A `Getter` works like a `Lens`, except you can only read from it, not write.

You can easily compose a `Getter` with a `Lens` just using `(.)`. No explicit coercion is necessary.

{% highlight haskell %}
ghci> ("hello",("world","!!!"))^._2._2.to length
3
{% endhighlight %}

As we saw above, you can write to lenses and these writes can change the type of the container. `(.~)` is an infix alias for `set`.

{% highlight haskell %}
ghci> _1 .~ "hello" $ ((),"world")
("hello","world)
{% endhighlight %}

It can be used in conjunction with `(&)` for a more familiar von Neumann style assignment syntax:

{% highlight haskell %}
ghci> ((), "world") & _1 .~ "hello"
("hello","world)
{% endhighlight %}

Conversely `view`, can be used as an prefix alias for `(^.)`.

{% highlight haskell %}
ghci> view _2 (10,20)
20
{% endhighlight %}

Lens comes "Batteries Included" with many lenses for manipulating common data types, such as Maps:

{% highlight haskell %}
ghci> import Data.Map as Map
ghci> Map.fromList [("hello","there")] ^.at "hello"
Just "there"
{% endhighlight %}

You can insert

{% highlight haskell %}
ghci> Map.fromList [("hello","there")] & at "hello" ?~ "world"
fromList [("hello","world")]
{% endhighlight %}

and delete with this lens

{% highlight haskell %}
ghci> Map.fromList [("hello","there")] & at "hello" .~ Nothing
fromList []
{% endhighlight %}

You can let the library automatically derive lenses for fields of your data type

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}
data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo
{% endhighlight %}

This will automatically generate the following lenses:

{% highlight haskell %}
bar, baz :: Simple Lens (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b
{% endhighlight %}

A `Lens` takes 4 parameters because it can change the types of the whole when you change the type of the part.

Often you won't need this flexibility, a `Simple Lens` takes 2 parameters, and can be used directly as a `Lens`.

Just like how we can write a `Getter` that can only be used for retrieving information, we can write a `Setter`
which can only be used for updates. Like a using `fmap` on a `Functor` updating a `Setter` can modify multiple
targets!

The canonical example of a setter is 'mapped':

{% highlight haskell %}
mapped :: Functor f => Setter (f a) (f b) a b
{% endhighlight %}

`over` is then analogous to `fmap`, but parameterized on the Setter.

{% highlight haskell %}
ghci> fmap succ [1,2,3]
[2,3,4]
ghci> over mapped succ [1,2,3]
[2,3,4]
{% endhighlight %}

The benefit is that you can use any `Lens` as a `Setter`, and the composition of setters with other setters or lenses using `(.)` yields
a `Setter`.

{% highlight haskell %}
ghci> over (mapped._2) succ [(1,2),(3,4)]
[(1,3),(3,5)]
{% endhighlight %}

`(%~)` is an infix alias for 'over', and the precedence lets you avoid swimming in parentheses:

{% highlight haskell %}
ghci> ([(42, "hello")],"world") & _1.mapped._2.mapped %~ succ
([(42, "ifmmp")],"world")
{% endhighlight %}

There are a number of combinators that resemble the `+=`, `*=`, etc. operators from C/C++ for working with the monad transformers.

There are `+~`, `*~`, etc. analogues to those combinators that work functionally, returning the modified version of the structure.

{% highlight haskell %}
ghci> (1,2) & both *~ 2
(2,4)
{% endhighlight %}

There are combinators for manipulating the current state in a state monad as well

{% highlight haskell %}
fresh :: MonadState Int m => m Int
fresh = id <+= 1
{% endhighlight %}

Anything you know how to do with a `Foldable` container, you can do with a `Fold`

{% highlight haskell %}
ghci> :m + Data.Char Data.Text.Lens
ghci> allOf (folded.text) isLower ["hello"^.packed, "goodbye"^.packed]
True
{% endhighlight %}

There are actually a large number of variations on the concept of a `Lens` provided by the library, in particular a `Traversal`
generalizes `traverse` from `Data.Traversable`.

You can also use this for generic programming. Combinators are included that are based on Neil Mitchell's `uniplate`, but which
have been generalized to work on or as lenses, folds, and traversals.

{% highlight haskell %}
ghci> :m + Data.Data.Lens
ghci> anyOf biplate (=="world") ("hello",(),[(2::Int,"world")])
True
{% endhighlight %}

As alluded to above, anything you know how to do with a `Traversable` you can do with a `Traversal`.

{% highlight haskell %}
ghci> mapMOf (traverse._2) (\xs -> length xs <$ putStrLn xs) [(42,"hello"),(56,"world")]
"hello"
"world"
[(42,5),(56,5)]
{% endhighlight %}

Moreover, many of the lenses supplied are actually isomorphisms, that means you can use them directly as a lens or getter:

{% highlight haskell %}
ghci> let hello = "hello"^.packed
"hello"
ghci> :t hello
hello :: Text
{% endhighlight %}

but you can also flip them around and use them as a lens the other way with `from`!

{% highlight haskell %}
ghci> hello^.from packed.to length
5
{% endhighlight %}

You can automatically derive isomorphisms for your own newtypes with `makeIso`. e.g.

{% highlight haskell %}
newtype Neither a b = Neither { _nor :: Either a b } deriving (Show)
makeIso ''Neither
{% endhighlight %}

will automatically derive

{% highlight haskell %}
neither :: Iso (Neither a b) (Neither c d) (Either a b) (Either c d)
nor :: Iso (Either a b) (Either c d) (Neither a b) (Neither c d)
{% endhighlight %}

such that

{% highlight haskell %}
from neither = nor
from nor = neither
neither.nor = id
nor.neither = id
{% endhighlight %}

### Field Guide

[![Lens Hierarchy](https://s3.amazonaws.com/creately-published/h5nyo9ne1)](https://creately.com/diagram/h5nyo9ne1/LBbRz63yg4yQsTXGLtub1bQU4%3D)

### Carrying On

There is also a fully operational, but simple game of [Pong](https://github.com/ekmett/lens/blob/master/examples/Pong.hs) in the [examples/](https://github.com/ekmett/lens/blob/master/examples/) folder.

There are also a couple of hundred examples distributed throughout the haddock documentation.
