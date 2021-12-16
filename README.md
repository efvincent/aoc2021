Advent of Code 2021
===================

This year I'm doing Haskell again, it's still my favorite for all sorts of reasons, but that's another story. I've branched the (excellent) advent of code [scaffolding](https://github.com/mstksg/advent-of-code-dev) project from [mstksg](https://github.com/mstksg) to use as a foundation for this year's repo. This scaffolding wraps up the [AoC API Haskell client](https://hackage.haskell.org/package/advent-of-code-api), which allows me to download the prompts, test, and submit code from `ghci`. It also provides nice types and utility functions for specifying the parser and solver functions consistently across all the puzzles. Have a look, and thank you Justin Le for this great contribution to the Advent of Code and Haskell communities!

On to the Advent of Code 2021

|       | Title                   | Prompt 1 | Prompt 2 | Code | 
|-------|-------------------------|:--------:|:--------:|:----:|
| [Day 1](#day-1--sonar-sweep)       | Sonar Sweep   | [x](./prompt/01a.md) | [x](./prompt/01b.md) | [x](./src/AOC/Challenge/Day01.hs) |
| [Day 2](#day-2--dive) | Dive!      | [x](./prompt/02a.md) | [x](./prompt/02b.md) | [x](./src/AOC/Challenge/Day02.hs) |
| [Day 3](#day-3--binary-diagnostic) | Binary Diagnostic    | [x](./prompt/03a.md) | [x](./prompt/03b.md) | [x](./src/AOC/Challenge/Day03.hs) |
| Day 4 | Giant Squid                | [x](./prompt/04a.md) | [x](./prompt/04b.md) | [x](./src/AOC/Challenge/Day04.hs) |
| Day 5 | Hydrothermal Venture       | [x](./prompt/05a.md) | [x](./prompt/05b.md) | [x](./src/AOC/Challenge/Day05.hs) |
| Day 6 | Lanternfish                | [x](./prompt/06a.md) | [x](./prompt/06b.md) | [x](./src/AOC/Challenge/Day06.hs) |
| Day 7 | The Treachery of Whales    | [x](./prompt/07a.md) | [x](./prompt/07b.md) | [x](./src/AOC/Challenge/Day07.hs) |
| Day 8 | Seven Segment Search       | [x](./prompt/08a.md) | [x](./prompt/08b.md) | [x](./src/AOC/Challenge/Day08.hs) |
| Day 9 | Smoke Basin                | [x](./prompt/09a.md) | [x](./prompt/08b.md) | [x](./src/AOC/Challenge/Day09.hs) |
| Day 10| Syntax Coloring            | | | |
| Day 11| Dumbo Octopus              | | | |
| Day 12| Passage Pathing            | | | |
| Day 13| Transparent Origami        | | | |
| Day 14| Extended Polymerization    | | | |
| Day 15| Chiton                     | | | |
| Day 16| Packet Decoder             | | | |

---
## Day 3 : Binary Diagnostic

[Prompt](./prompt/03a.md) / [Code](.src/AOC/Challenge/Day03.hs)

For day 3, we find our submarine in need of some diagnostics, which can only be found by decoding a cryptic series of binary digits. Because of course!

The first diagnostic we need is the power consumption which we find by multiplying a *gamma rate* by an *epsilon rate*. Gamma is found by finding the most common bit in the correspoinding position of all the numbers in the diagnostic, and epsilon is found by finding the least common bit in each position.

```
  00100
  11110
  10110
  10111
```

In this truncated example, the most common bit in the first position is `1`, in the second is `0`, etc., for a gamma output of `10110`. Epsilon is the logical not of each bit since its looking for the least common. 

```haskell
solve1 :: [[Bool]] -> Int
solve1 values =
  bToi g * bToi e
  where
    l = (length . head $ values) - 1
    g = map (colWise values) [l, (l -1) .. 0]
    e = map not g
```

The algorithm I used for part 1 is to determine the bits in gamma, then the bits in epsilon, then convert them to integers and multiply them. Gamma is found by mapping a list from `l` to `0` where `l` is the largest index of one of the lines. In our sample, `l` would be 4. For each of these indexes, we get the most common bit at that position across all values by "pivoting" the list of lists and using the index as a column index. The result is a list of most common bits.

```haskell
colWise :: [[Bool]] -> Int -> Bool
colWise xs idx =
  (>= majority xs) . length . filter id . map (!! idx) $ xs
```
The `colWise` function takes a list of lists of Bool `[[a]]` and maps each of the member lists `[a]` with the function `(!! idx)`, which changes the list to value of the member of the list at index `idx`. The `filter` function filters the list to only where the predicate is true. The predicate we're using is `id`, which just returns the value.

Since each value in the list is a boolean, filter eliminates all the `False` values, leaving the number of `True` values. Next

Epsilon is the inverse of epsilon, so we can `map not` across gamma. Finally the `bToi` function converts both the gamma and epsilon bits to `Int`s. We multiply these together to solve part 1!

```haskell
bToi :: [Bool] -> Int
bToi = foldr (\bit acc -> fromEnum bit + 2 * acc) 0
```
The `foldr` function is a right associative fold which traverses from left to right (see the [docs](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:foldr) for deeper explanation). The folding function starts the accumulator at `0` and uses `fromEnum` to convert the bit to an integer and apply the logic to accumulate the integer from the list of bits. In this function `acc` is the accumulator.

#### Techniques
**Generating a list** from start and endpoints where the "step" is not 1 has a strange syntax in Haskell compared to other languages. You need to indicate the first value, _and the second value_, and then the elipsis and the last value: `[l, (l - 1) .. 0]`

**Function composition:** A very common technique in Haskell is to _compose_ two functions together to create a new function. You see this in a few different plces. In `solve1`, the value of `l` is given as ```l = (length . head $ values) - 1``` 

The function `head :: [a] -> a` returns the first element in a list, and `length :: [a] -> Int` returns the length of a list. The composition function has type 

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Which looks daunting, but all it's saying is take a function `b -> c` and second function `a -> b`, and return a function `a -> c`. It does this by taking the output of the second function and feeding it to the first function. Imagine we had these functions:

```haskell
buyFood :: Money -> Ingredients
makePizza :: Ingredients -> Food
rateFood :: Person -> Food -> Rating
```

A conventional approach to calling these might look like this

```javascript
var funds = 100
var ingredients = buyFood(funds)
var pizza = makePizza(ingredients)
var rating = rateFood("Giacomo", pizza)
console.log(rating)
```

This creates a bunch of temporary variables. Or you could do this:

```javascript
console.log(rateFood("Giacomo", makePizza(buyFood(funds))))
```
Better. But not great. In Haskell, you'd likely see this (ignore that `log` probably does I/O for now):

```haskell
log . rateFood "Giacomo" . makePizza . buyFood $ funds
```
The functions are "composed" together. The output of `buyFood` is the input to `makePizza`, and the output of that is the input to `rateFood "Giacomo"`. Keep in mind that `rateFood` takes both a person (string) and a `Food`. Thanks to partial function application, the expression `rateFood "Giacomo"` results in a new function that has the type `Food -> Rating`, and that is composed with the output of `makePizza`. You still have to read this from right to left, but this becomes second nature, and function composition is useful for other reasons as well.

### Part 2

That was a lot of "technique"... anyway, part 2 has us seeking the _life support rating_ and the _oxygen generator rating_. I'll not be retyping the entire description, have a look at the [prompt](./prompt/03b.md). The solution:

```haskell
solve2 :: [[Bool]] -> Int
solve2 values =
  let ox = go id 0 values in
  let co = go not 0 values in
  ox * co
  where
    go :: (Bool -> Bool) -> Int -> [[Bool]] -> Int
    go f i xs =
      let predicate v = 
            (== (v !! i)) 
            . f 
            . (>= majority xs) 
            . length 
            . filter (!! i) $ xs in
        
      case filter predicate xs of
        [bs] -> bToi . reverse $ bs
        bss  -> go f (i + 1) bss
```
At this point, if you read the prompt you should be able to figure out what part 2 is doing... I'm off to get some dinner! 😄

## Day 2 : Dive!

[Prompt](./prompt/02a.md) / [Code](./src/AOC/Challenge/Day02.hs)

### Part 1
In day 2's *Dive!* challenge, we're given a series of instructions for navigating our submarine, as shown in the sample data:

```
forward 5
down 5
forward 8
up 3
down 8
forward 2
```
We define a type to describe an instruction, and one for the current position. The _forward_ instruction increases horizontal position, _up_ and _down_ effect depth, so have a type to track position. It's then a matter of folding over the instructions accumulating the effect on position.

```haskell
type Instr = (Char, Int)

type Pos = (Int, Int)

parse :: String -> [Instr]
parse s = go [] (map (splitOn " ") . lines $ s)
  where
    go ins [] = reverse ins
    go ins ([dir, v] : rest) = go ((head dir, read v) : ins) rest
    go _ _ = error "Bad input"

solve1 :: Pos -> [Instr] -> Int
solve1 (h, d) = \case
  ('f', n) : rest -> solve1 (h + n, d) rest
  ('d', n) : rest -> solve1 (h, d + n) rest
  ('u', n) : rest -> solve1 (h, d - n) rest
  _ -> h * d
```

The strategy for parsing the input is typical, I won't describe parsing in detail beyond this point unless some interesting parsing is required. In this case we split the lines into a list of strings, and for each string we build a tuple of the first character of the instruction and the amount, appending to a list. 

#### Techniques
* In the `parse` function, by appending to the head of the list we need to reverse the final list. This is an inefficiency but we're not looking for optimal performance, just reasonable performance, and in this case I find this approach canonical and readable.
* The final case of `go` in `parse` throws an error. We don't expect this to happen, as we know the format of the input and it will always have one of the two initial `go` cases. But to satisfy GHC's desire to have all cases handled, we explicitly create the case to handle the unexpected scenario.
* Note the use of the `{-# LANGUAGE LambdaCase #-}` [language extension](https://wiki.haskell.org/Language_extensions). This allows for a more compact case syntax that enables you to write a case statement for the implied last parameter of the function. This is very similar to the [`function` keyword](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions) in F#. See [this example](https://riptutorial.com/haskell/example/5689/lambdacase) for a more detailed explanation.

### Part 2

In part 2 the interpretation of the instruction changes. Rather than up and down directly effecting the depth, they effect what the puzzle describes as the _aim_. Aim effects depth whenever the sub moves horizontally. For each horizontal unit moved, depth is moved that many units multiplied by aim.

```haskell
solve2 :: Int -> Pos -> [Instr] -> Int
solve2 a (h, d) = \case
  ('f', n) : rest -> solve2 a (h + n, d + (a * n)) rest
  ('d', n) : rest -> solve2 (a + n) (h, d) rest
  ('u', n) : rest -> solve2 (a - n) (h, d) rest
  _ -> h * d
```
To implement this, we need to track aim as another parameter of the `solve2` function. As we fold over the instructions we'll adjust aim when we see up / down instructions. Parsing is the same for part 2 as for part 1, as it has been so far and how we endeavor for it to be for each day in AoC.

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

[Prompt](./prompt/01a.md)

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





