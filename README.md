# FP Plays Chess

## Compile and run

- `cabal build`
- `cabal run haskell-chess -- [THEME] [MODE] [Optional FEN]`
  - THEME:
    - 'lighttheme'
    - 'darktheme'
  - MODE:
    - 'humanvshuman'
    - 'humanvsstockfish' (requires `stockfish` to be installed)
    - 'stockfishvsstockfish' (requires `stockfish` to be installed)
    - 'stockfishvsstockfishquick' (requires `stockfish` to be installed)
  - Eg: `cabal run haskell-chess -- darktheme stockfishvsstockfish "r1b2rk1/pp2qppp/2nbpn2/2pp4/3P4/2PBPNB1/PP1N1PPP/R2Q1RK1 b - - 8 9"`
- `cabal run haskell-chess` defaults to lighttheme, humanvshuman, from the normal starting position.

## Playing against an engine

### Stockfish

- The `stockfish` executable must be installed and on your `$PATH`
  - On Ubuntu/WSL: `sudo apt install stockfish`
  - On Mac: `brew install stockfish`
