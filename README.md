blokus
======

AI for the board game Blokus

### status
Unfinished, unlikely to be finished, mostly just used for learning Haskell. The code to find legal moves from an arbitrary game state is working, as are various other pieces, but there is no interface to play a game, nor is there AI of usable sophistication.

If you're interested in something like this, check out [Pentobi](http://pentobi.sourceforge.net/).

### building
If you have a recent enough Haskell Platform installation, just:
```bash
  git clone git@github.com:djudd/blokus.git
  cd blokus
  cabal configure
  cabal build
```

test.sh and profile.sh scripts are also included.
