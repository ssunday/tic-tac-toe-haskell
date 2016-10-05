# Tic Tac Toe

Haskell Tic Tac Toe Game. Glorious. So pure it is impure.

## Features

* Player vs. AI
* You can choose your marker. AI and Player get different colors.
* Choose who makes the first move
* Play game over and over again
* Menu system to display scores or play the game
* Two board types: 3x3 and 4x4
* Display colors
* Scoring saves to either txt file or PG database. Defaults to txt file

## Usage

To Play:

1. Install Haskell and Cabal
2. Clone Repo `git clone https://github.com/the-real-makuta/tic-tac-toe-haskell.git`
3. Enter directory
4. Run `cabal install` and `cabal configure` as needed
5. `cabal run`

For Postgres scoring:

1. Have Postgres running locally with 'postgres' user
2. Run `. bin/setup.sh`.
3. To use it when running, use `cabal run pg`

## Testing

Make sure to run `cabal install --enable-test`, and if cabal balks, `cabal configure`

Using cabal (faster):

```
cabal test
```

For colored test output, but slower execution:

```
runhaskell -isrc -itest test/Spec.hs
```
