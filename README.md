# CCCaTT₁

A type theory for **unbiased cartesian closed categories**. We provide a small proof assistant which you can also [try online](https://smimram.github.io/cccatt/). In order to get used to the syntax, you can have a look at the [test file](tests/test.cccatt).

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
