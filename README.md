# jewels-haskell

This is a Haskell implementation of the jewels game [described
here](https://github.com/silverfish707/jewels).  I've made no attempt to
make it run quickly, so my Python implementation beats it easily (even
when compiled) - the point was to learn a little about mutable arrays
using `IOArray`.  I hope it will be useful to anyone else trying to get
their head around those.

### Instructions

Compile `jewels2.hs` with `ghc jewels2.hs`, then run `./jewels2 10` to
have it play through 10 games. Output is of the form `(mean, sd)` where
`mean` is the mean score and `sd` the standard deviation.
