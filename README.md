# Conway

A quick conway's game of life implementation using Representable and Comonad.

# Running

* Clone the repo
* `stack build && stack exec conway-exe`
* ???
* Profit!

# Customization
* Clone the repo
* Define your own shapes as a list of Coordinates (see [`glider`](./src/Conway.hs) as an example)
* Define your own 'life' rules as a function from `Grid Bool -> Bool`; the grid passed to the function is focused on
    the cell you should compute 'life' for.
* Customize your tick time
* Build a starting grid using provided combinators, see [`start`](./app/Main.hs) as an example.
