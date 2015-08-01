# General
This is implementation of a basic Reversi AI-Player including:
- Negamax with alpha-beta pruning
- Static board weights for different stages of the game
- Heauristics:
  - Chips
  - Mobility
  - Position
  - Stability


The main data structure of choice is a 2D-Array. It should give
slightly better performance than the pure list implementation, 
while still being relatively easy to work with (unlike a Bitboard for example).

Near the end of the game the algorithm increases its recursion depth and
tries to predict the outcome before the opponent.

# Ideas for improvement
The implementation was done within a few days for a time constrained challenge. There are many things which can be improved. Here are some of the ideas for further development:
- Bitboards
- Transposition Tables
- Move Ordering
- Variation caching
- Opening book
- Endgame book

# Compile & Play
You can play against the AI to test it out. 
This should be seen as simple demonstration and not a real implementation of the game. You are expected to place your chips only on legal positions (no input validation implemented).

### Prerequisites
[Haskell Platform](https://www.haskell.org/platform/)
### Compile
    ghc -o reversi Main.hs
### Run
    ./reversi
### Play
``` 
  0 1 2 3 4 5 6 7
 +-+-+-+-+-+-+-+-+
0| | | | | | | | |0
 +-+-+-+-+-+-+-+-+
1| | | | | | | | |1
 +-+-+-+-+-+-+-+-+
2| | | | | | | | |2
 +-+-+-+-+-+-+-+-+
3| | | |O|X| | | |3
 +-+-+-+-+-+-+-+-+
4| | | |X|O| | | |4
 +-+-+-+-+-+-+-+-+
5| | | | | | | | |5
 +-+-+-+-+-+-+-+-+
6| | | | | | | | |6
 +-+-+-+-+-+-+-+-+
7| | | | | | | | |7
 +-+-+-+-+-+-+-+-+
  0 1 2 3 4 5 6 7
You are playing with (X). Enter coordinates:
4
5

  0 1 2 3 4 5 6 7
 +-+-+-+-+-+-+-+-+
0| | | | | | | | |0
 +-+-+-+-+-+-+-+-+
1| | | | | | | | |1
 +-+-+-+-+-+-+-+-+
2| | | | | | | | |2
 +-+-+-+-+-+-+-+-+
3| | | |O|O|O| | |3
 +-+-+-+-+-+-+-+-+
4| | | |X|X| | | |4
 +-+-+-+-+-+-+-+-+
5| | | | |X| | | |5
 +-+-+-+-+-+-+-+-+
6| | | | | | | | |6
 +-+-+-+-+-+-+-+-+
7| | | | | | | | |7
 +-+-+-+-+-+-+-+-+
  0 1 2 3 4 5 6 7
Your score: 3	Score AI: 3
You are playing with (X). Enter coordinates:
..
..
```
