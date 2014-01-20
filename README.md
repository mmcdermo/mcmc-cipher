mcmc-cipher
===========

Breaking Classical Ciphers with Markov Chain Monte Carlo

Usage
```
cabal configure
cabal build
./dist/build/mcmc-cipher/mcmc-cipher cipherFile { -ma | -vg } chainIterations
```
*-ma / -vg*: Choose between Monoalphabetic Substitution cipher and Vignere cipher

*chainIterations*: Number of iterations to run MCMC for (default 10000)