Advent of Code 2021
===================

This year I'm doing Haskell again, it's still my favorite for all sorts of reasons, but that's another story. I've branched the (excellent) advent of code [scaffolding](https://github.com/mstksg/advent-of-code-dev) project from [mstksg](https://github.com/mstksg) to use as a foundation for this year's repo. This scaffolding wraps up the [AoC API Haskell client](https://hackage.haskell.org/package/advent-of-code-api), which allows me to download the prompts, test, and submit code from `ghci`. It also provides nice types and utility functions for specifying the parser and solver functions consistently across all the puzzles. Have a look, and thank you Justin Le for this great contribution to the Advent of Code and Haskell communities!

On to the Advent of Code 2021

|       | Title                   | Prompt 1 | Prompt 2 | Code | 
|-------|-------------------------|:--------:|:--------:|:----:|
| [Day 1](#day-1--sonar-sweep) | Sonar Sweep   | [x](./prompt/01a.md) | [x](./prompt/01b.md) | [x](./src/AOC/Challenge/Day01.hs) |
| Day 2 | Dive!                   | [x](./prompt/02a.md) | [x](./prompt/02b.md) | [x](./src/AOC/Challenge/Day02.hs) |
| Day 3 | Binary Diagnostic       | [x](./prompt/03a.md) | [x](./prompt/03b.md) | [x](./src/AOC/Challenge/Day03.hs) |
| Day 4 | Giant Squid             | [x](./prompt/04a.md) | [x](./prompt/04b.md) | [x](./src/AOC/Challenge/Day04.hs) |
| Day 5 | Hydrothermal Venture    | [x](./prompt/05a.md) | [x](./prompt/05b.md) | [x](./src/AOC/Challenge/Day05.hs) |
| Day 6 | Lanternfish             | [x](./prompt/06a.md) | [x](./prompt/06b.md) | [x](./src/AOC/Challenge/Day06.hs) |
| Day 7 | The Treachery of Whales | [x](./prompt/06a.md) | [x](./prompt/06b.md) | [x](./src/AOC/Challenge/Day06.hs) |

---

## Day 1 : Sonar Sweep

[Prompt](./prompt/01a.md) / [Code](./src/AOC/Challenge/Day01.hs)

### Part One
A simple rolling slice of two elements of the list of inputs, comparing the first and second. The
accumulator is incremented whenever the second is larger than the first. 

#### Techniques
* Use of the internal `go` function, which I found last year during AoC seems to be a common way to name an internal looping recursive function.
* Point free - the `solve1` function doesn't have a formal parameter, rather it is set to the curried `go` function which is passed just one of the to parameters it's required - the `n` which is the seed for the fold. the type of `go` is `Int -> [Int] -> Int`, and because the first `Int` is passed `0`, the type of `go 0` is `[Int] -> Int`. This is exactly what we need for `solve1`.

```haskell
solve1 :: [Int] -> Int
solve1 = go 0
  where
    go n (a:b:rest) = if b > a then go (n+1) (b:rest) else go n (b:rest)
    go n _ = n 
```

### Part Two
This is just a bit more complex than part 1, requiring that we look at the sum of a three element rolling
window, carrying the last three element sum forward. If the previous exists (ie we're at least at the forth element) 
and it's less than the current, then we increment the accumulator. Either way we carry forward a new previous.

#### Techniques
* Since the previous might not exist, we use a `Maybe Int` where `Nothing` indicatest there is no previous window of three elements, and `Just p` means there is a previous.
* We pattern match using `(a:b:c:rest)` which will only match a list with at least three elements.
* `go` uses pattern matching to define one case where there are at least three elements, and a second definition handles any other case. When there are not at least three elements, we return `n` the accumulated answer
```haskell
solve2 :: [Int] -> Int
solve2 = go 0 Nothing
  where
    go n prv (a:b:c:rest) = 
      let cur = a+b+c in
      case prv of
        Nothing -> go n (Just cur) (b:c:rest)
        Just p -> if cur > p then go (n+1) (Just cur) (b:c:rest) else go n (Just cur) (b:c:rest)
    go n _ _ = n
```





