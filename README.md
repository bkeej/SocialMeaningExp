Some tools for probabilistic calculating and linguistic applications. Uses the Probability monads detailed by [Erwig & Kollmansberger (2006)][1] and expanded on by Eric Kidd ([here][2], [here][3], and [here][4]).

Super preliminary for now --- these are more learning tools for me than
scientific tools for you. Seems like the only way I can learn new formal
things these days is by forcing myself to code up an implementation. 🤫

Some things I'll be adding in the near future:

-   Costs and temperatures (may require `listen` and `tell` like
    functionality, making the underlying monadic structures even more
    `Writer`-y).

-   Variable lexica/lexical uncertainty, a la [Potts et al (2015)][5].

-   Seeing whether there's any abstraction underlying the various RSA models
    (or other kinds of formal pragmatic models, e.g., IBR).

-   A treatment of the [scalar implicatures of exceptionally scoping
    indefinites][6]. I think the lexical uncertainty models may work nicely
    here, but I'll need to check.
    
[1]: https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
[2]: http://www.randomhacks.net/2007/02/21/refactoring-probability-distributions/
[3]: http://www.randomhacks.net/files/build-your-own-probability-monads.pdf
[4]: https://github.com/emk/haskell-probability-monads
[5]: https://academic.oup.com/jos/article-abstract/33/4/755/2563037
[6]: http://ling.auf.net/lingbuzz/003181
