# Five Five-Letter Words

A Haskell solution to the problem of finding all sets of five disjoint five-letter words in a given word list, motivated by [this video](https://www.youtube.com/watch?v=c33AZBnRHks).

My solution uses the following:

1. Recursion;
1. Bitwise representation of letter sets;
1. Removal of letter sets containing used letters;
1. Pruning based on:
    1. The fact that a valid solution can only have one unused letter, meaning that you can prune the search when you identify two unused letters that are not in any unused letter sets;
    1. Sorting the letter sets in order of increasing minimal letter frequency, so that this pruning method is equivalent to pruning at the first unused letter set that contains neither of the two least frequent unused letters (letter order is used to break frequency ties).

# Build

```
cabal build
```

# Run

```
cabal run
```
