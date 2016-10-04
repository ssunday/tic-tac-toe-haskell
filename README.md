# Tic Tac Toe

Haskell Tic Tac Toe Game. Glorious. So pure it is impure.

* Player vs AI
* You can choose your marker
* Choose who goes first
* Play game over and over again
* Colors
* Scoring with either TXT or PG. Defaults to TXT.

## Usage

Have Haskell + Cabal installed before attempting this or else it won't work.

For Postgres scoring:

1. Have postgres running.
2. Run `. bin/setup.sh`.
3. To use it when running, use `cabal run pg`

Clone to desktop and enter the directory. Run `cabal install`, `cabal configure` as needed, and then:

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
