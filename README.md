# Tic Tac Toe


Haskell Tic Tac Toe Game. Glorious. So pure it is impure.

* Player vs AI
* You can choose your marker

## Usage

Have Haskell + Cabal installed before attempting this or else it won't work.

Clone to desktop and enter the directory. Run `cabal install`, configure as needed, and then:

```
cabal run
```

## Testing

Make sure to run `cabal install --enable-test` just in case.

Using cabal (faster):

```
cabal test
```

For pretty output (color), but slower:

```
runhaskell -isrc -itest test/Spec.hs
```
