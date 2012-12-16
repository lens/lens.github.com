---
layout: post
category: wiki
tags: [link, tutorial, article, wiki]
title: "Varying lens properties by instance"
link: https://github.com/ekmett/lens/wiki/Varying-lens-properties-by-instance
author: Adrian Keet
---

Say we have a few types for things that have a "name", which is a `String` that may or may not be modifiable. We want to have a type class for these things that exposes a lens-like method for accessing the name in a type-safe manner, so that it may be a Lens or just a Getter depending on the instance.

First let's write some example types and some lenses for them:

{% highlight haskell %}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

import Control.Lens
import GHC.Exts -- for the Constraint kind

newtype ReadWrite = ReadWrite String
newtype ReadOnly = ReadOnly String

       -- Functor f => SimpleLensLike f ReadWrite String
rwName :: SimpleLens ReadWrite String
rwName f (ReadWrite s) = ReadWrite `fmap` f s

       -- Gettable f => SimpleLensLike f ReadWrite String
roName :: Getter ReadOnly String
roName = to (\(ReadOnly a) -> a)
{% endhighlight %}

Now `rwName` is a full Lens, while `roName` can only be used as a Getter. We'd like to make a single class that lets us get and set the name of a `ReadWrite`, but only get the name of a `ReadOnly`.

Using a multi-parameter type class
==================================
Now the only difference between a Getter and a Lens is the restriction on the type of Functor it can be used with. So, the first idea is to let our class take that Functor as a parameter:

{% highlight haskell %}
class NamedMPTC f a where
  nameMPTC :: SimpleLensLike f a String

instance Functor f => NamedMPTC f ReadWrite where
  nameMPTC = rwName

instance Gettable f => NamedMPTC f ReadOnly where
  nameMPTC = roName
{% endhighlight %}

And now `nameMPTC` gives us a type-safe way to get what we want from both `ReadOnly` and `ReadWrite`:

{% highlight haskell %}
ghci> ReadWrite "a" ^. nameMPTC
"a"
ghci> ReadWrite "a" & nameMPTC .~ "b"
ReadWrite "b"
ghci> ReadOnly "c" ^. nameMPTC
"c"
ghci> ReadOnly "c" & nameMPTC .~ "d"
-- No instance for (Gettable Mutator) arising from a use of `nameMPTC' ...
{% endhighlight %}

This appears to work pretty well. We can easily restrict to different types of Functors; for example, restricting to Applicative will make `nameMPTC` a Traversal, in case we had something with multiple names.

When we try to use this in practice, however, we run into problems. If `NamedMPTC` had another method that made no reference to `f`, then any time we tried to use it, the type checker would blow up with an ambiguous type error! The only way around this would be to have our lens-like method be in its own class, which sounds pretty undesirable. What we needed was a way to impose the constraint on `f` *only* on the lens-like method, not the entire class. So let's forget this and look at another approach.

Using the Constraint kind
=========================
This means we'll need at least GHC 7.4. Let's see the code first:

{% highlight haskell %}
class Named a where
    type NameConstraint a f :: Constraint
    name :: NameConstraint a f => SimpleLensLike f a String

instance Named ReadWrite where
    type NameConstraint ReadWrite f = Functor f
    name = rwName

instance Named ReadOnly where
    type NameConstraint ReadOnly f = Gettable f
    name = roName
{% endhighlight %}

Let's see how the types work. First let's look at the type of `name`:

{% highlight haskell %}
name :: (NameConstraint a f, Named a) => SimpleLensLike f a String
{% endhighlight %}

When we specialize to `a = ReadOnly`, the `Named a` constraint is satisfied and `NameConstraint a f` just becomes `Gettable f`, so we end up with

{% highlight haskell %}
name :: Gettable f => SimpleLensLike f ReadOnly String
{% endhighlight %}
which is exactly the type of `roName` we had before.

With this approach, `name` works exactly how `nameMPTC` did earlier. But notice that this time the constraint on `f` is only applied to `name`, not the entire class. This means we can easily add other methods to the class without causing any ambiguous type issues, and we can even add different lens-like methods with different constraints. Fantastic!
