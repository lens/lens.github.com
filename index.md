---
layout: site
title: Home
---


This package provides families of [lenses](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Type.hs), [isomorphisms](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Iso.hs), [folds](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Fold.hs), [traversals](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Traversal.hs), [getters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Getter.hs) and [setters](https://github.com/ekmett/lens/blob/master/src/Control/Lens/Setter.hs).

<blockquote>
  <p>Costate Comonad Coalgebra is equivalent of Java's member variable update technology for Haskell</p>
  <small><a href="https://twitter.com/PLT_Borat/">@PLT_Borat</a> <cite title="twitter">on twitter</cite></small>
</blockquote>

The [FAQ](https://github.com/ekmett/lens/wiki/FAQ), which provides links to a large number of different resources for learning about lenses and an overview of the [derivation](https://github.com/ekmett/lens/wiki/Derivation) of these types can be found on the [Lens Wiki](https://github.com/ekmett/lens/wiki) along with a brief [overview](https://github.com/ekmett/lens/wiki/Overview) and some [examples](https://github.com/ekmett/lens/wiki/Examples).

Documentation is available through [github](http://ekmett.github.com/lens/frames.html) (for HEAD) or [hackage](http://hackage.haskell.org/package/lens) for the current and preceding releases.

### Recent Activity

<div class="content">
  <div class="related">
    <ul>
      {% for post in site.posts %}
      <li>
        {% if post.link != null %}
        <span>{{ post.date | date: "%B %e, %Y" }}</span> &middot; <span><a href="{{ post.link }}">{{ post.title }}</a></span> &middot; <span>{{ post.teaser }}</span>
        {% else %}
        <span>{{ post.date | date: "%B %e, %Y" }}</span> &middot; <span><a href="{{ post.url }}">{{ post.title }}</a></span> &middot; <span>{{ post.teaser }}</span>
        {% endif %}
      </li>
      {% endfor %}
    </ul>
  </div>
</div>


