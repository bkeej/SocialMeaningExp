Some tools for probabilistic calculating and linguistic applications. Uses the
Probability monads detailed by [Erwig & Kollmansberger (2006)][1] and expanded
on by Eric Kidd ([here][2], [here][3], and [here][4]).

Super preliminary for now --- these are more learning tools for me than
scientific tools for you. Seems like the only way I can learn new formal things
these days is by making myself code up an implementation. 🤫

`Util.hs` has the basic monad transformer for probabilistic computing (coarser
semirings are acceptable too). `Prob.hs` uses this transformer to define
various probabilistic monads, with support for Bayesian calculations and
sampling functions. All of this is based pretty directly on Kidd's work.

`RSA.hs` implements a simple Rational Speech Acts model using these tools.
`Scalar.hs` extends this to scalar implicature with lexical uncertainty (and
message costs).

Some things I'm thinking about:

-   Embedded implicature, à la [Potts et al (2015)][5].

-   Helping yourself to lexical scales vs. not (cf. Potts et al again, as well
    as [Russell 2012][6]).

-   Abstractions underlying the various RSA models (or other kinds of formal
    pragmatic models, e.g., IBR). Is there a most-general perspective?

-   The [scalar implicatures of exceptionally scoping indefinites][7]. I think
    the lexical uncertainty models may work nicely here, but I should check.

-   How (normalizable) semirings coarser than probabilities might be useful.

[1]: https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
[2]: http://www.randomhacks.net/2007/02/21/refactoring-probability-distributions/
[3]: http://www.randomhacks.net/files/build-your-own-probability-monads.pdf
[4]: https://github.com/emk/haskell-probability-monads
[5]: https://academic.oup.com/jos/article-abstract/33/4/755/2563037
[6]: http://semanticsarchive.net/Archive/WY1YTRhM/
[7]: http://ling.auf.net/lingbuzz/003181
