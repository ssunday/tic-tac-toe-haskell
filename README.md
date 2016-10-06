# Tic Tac Toe

Haskell Tic Tac Toe Game. Glorious. So impure it is pure.

## Features

* Player vs. AI
* You can choose your marker. AI and Player get different colors.
* Choose who makes the first move
* Play game over and over again with different options
* Menu system to display scores or play the game
* Two board types: 3x3 and 4x4
* Display colors (depends on iTerm settings, may look different depending on color profiles)
* Scoring saves to either txt file or PG database. Defaults to txt file

## Usage

To Play:

1. [Install Haskell and Cabal](https://ghcformacosx.github.io/), run `cabal update` if this is your first time
2. Clone repo: `git clone https://github.com/the-real-makuta/tic-tac-toe-haskell.git`
3. Enter directory
4. Run `cabal install` and then `cabal configure`
5. To run app do: `cabal run`

For Postgres scoring rather than a .txt file:

1. Have Postgres running locally on default port with a 'postgres' user
2. Run `. bin/setup.sh`
3. To use it with the application, use `cabal run pg`

## Testing

Make sure to run `cabal install --enable-test` and `cabal configure`

Using cabal (faster):

```
cabal test
```

For colored test output, but slower execution:

```
runhaskell -isrc -itest test/Spec.hs
```
