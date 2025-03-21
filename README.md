# CCCaTT

A type theory for **unbiased cartesian closed categories** and variants with other structures (monoidal, cartesian, etc.) and with higher dimensions. We provide a small proof assistant which you can also [try online](https://smimram.github.io/cccatt/).

## Introduction

The general idea here is that this proof assistant allows you to define _coherences_ which are simply terms with given type. If those are accepted, you can be sure that the corresponding morphisms can be defined in any cartesian closed category (this is the correctness of our theory). Conversely, there is a way to obtain any morphism which is part of the structure of cartesian closed categories as a composite of coherences (this is the completeness part).

For instance, we expect to be able to define an identity on any object and, indeed, we can define

```
coh id (a : .) : a -> a
```

which can be thought of as a formal function which takes an object `a` and provides a morphism `a → a`. Similarly, we can define the composition of two morphisms by

```
coh comp {a b c : .} (f : a -> b) (g : b -> c) : a -> c
```

(arguments with curly braces are implicit ones). From there, we can define the doubling function which takes an endomorphism and composes it with itself:

```
let double {a : .} (f : a -> a) := comp f f
```

Note that the doubling function _cannot_ be obtained as a coherence and

```
coh double {a : .} (f : a -> a) : a -> a
```

will raise an error: intuitively, coherence should be unambiguous and, above, the typing is not informative enough to unambiguously determine if we are trying to define the double or the triple function for instance. Also, our system only allows defining functions which are present in any ccc. For instance, the definition

```
coh inv {a b : .} (f : a -> b) : b -> a
```

will be rejected (it would make sense if we were considering groupoids, which we are not). Since we are in cartesian categories, we can also derive projections and pairing

```
coh fst {a b : .} : a * b -> a
coh snd {a b : .} : a * b -> b
coh pairing {a b c : .} (f : a -> b) (g : a -> c) : a -> b * c
```

and the internal hom can be defined similarly.

Not only we can derive all the structural morphisms, but we can also derive all the equations that should hold between those. For instance, we expect identities to be neutral elements and, indeed, we can define:

```
coh id-left {a b : .} (f : a -> b) : comp (id a) f = f
coh id-right {a b : .} (f : a -> b) : comp f (id b) = f
```

similarly, we can derive equations related to cartesian closed structure, such that the fact that projecting allows recovering the components of the pairing:

```
coh pairing-fst {a b c : .} (f : a -> b) (g : a -> c) : comp (pairing f g) fst = f
coh pairing-snd {a b c : .} (f : a -> b) (g : a -> c) : comp (pairing f g) snd = g
```

You can explore categories with other structures by changing the _mode_. For instance, if you put

```
#-# mode: monoidal
```

at the beginning of your file, you switch to monoidal categories, where you can define tensoring

```
coh tens {a a' b b' : .} (f : a -> a') (g : b -> b') : a * b -> a' * b'
```

(which would also be accepted for cartesian categories for instance), but coherences such as `fst`, `snd` or `pairing` now get rejected. Some of the currently supported modes are

- categories
- monoidal categories
- symmetric monoidal categories
- symmetric monoidal closed categories
- cartesian categories
- cartesian closed categories

You can also change the dimension by typing something as

```
#-# dimension: 2
```

which will switch to bicategories (equipped with the structure corresponding to the current mode) and you can now define vertical composition as

```
coh vcomp {a b : .} {f g h : a -> b} (α : f -> g) (β : g → h) : f -> h
```

Have a look a the [examples](examples/) or the [online demo](https://smimram.github.io/cccatt/) for more illustrations.

## Theory

The theory is available in [this paper](http://www.lix.polytechnique.fr/Labo/Samuel.Mimram/docs/mimram_cccatt.pdf).

## Some examples

- [Cartesian closed categories](examples/ccc.cccatt)
- [Curry's axioms for λ-calculus](examples/curry.cccatt)
- [Curien's categorical combinators](examples/curien.cccatt)

## Emacs mode

The Emacs mode can be loaded by adding

```
(require 'cccatt-mode "~/path/to/cccatt-mode.el")
```

to you `.emacs` file.
