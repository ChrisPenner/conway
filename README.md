# Conway

A quick cli conway's game of life implementation using Representable and Comonad.

The board 'wraps around' Pac-man style.

# Running

* Clone the repo: `git clone https://github.com/ChrisPenner/conway.git`
* `stack build && stack exec conway-exe`
* ???
* Profit!

# Customization
* Clone the repo: `git clone https://github.com/ChrisPenner/conway.git`
* Define your own shapes as a list of Coordinates; e.g.

```haskell
glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]
```

* Define your own 'life' rules as a function from `Grid Bool -> Bool`; the grid passed to the function is focused on
    the cell you should compute 'life' for.
* Customize your tick time in [`Main.hs`](./app/Main.hs)
* Build a starting grid using provided combinators, e.g.

```haskell
grid :: Grid Bool
grid = mkGrid $
     glider `at` (0, 0)
  ++ blinker `at` (5, 10)
  ++ beacon `at` (15, 5)
```
