# Tic Tac Toe

Haskell Tic Tac Toe Game. Glorious. So pure it is impure.

* Player vs AI
* You can choose your marker
* Choose who goes first
* Play game over and over again
* Menu system
* Display colors
* Scoring saves to either txt file or PG database. Defaults to txt file

## Usage

Have Haskell + Cabal installed before attempting this or else it won't work.

Run `cabal install`, `cabal configure` as needed, and then:

```
cabal run
```

For Postgres scoring:

1. Have Postgres running locally with 'postgres' user
2. Run `. bin/setup.sh`.
3. To use it when running, use `cabal run pg`

## Testing

Make sure to run `cabal install --enable-test` just in case + `cabal configure`

Using cabal (faster):

```
cabal test
```

For pretty output (color), but slower:

```
runhaskell -isrc -itest test/Spec.hs
```
