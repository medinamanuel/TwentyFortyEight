# TwentyFortyEight

My solution in Scala to this challenge in CodeEval.
https://www.codeeval.com/open_challenges/194/

## Details:

Sliding the board means to slide each row or col depending on the move:

- UP or DOWN: slide rows
- LEFT or RIGHT: slide cols

Sliding a row or col means to move it to the given direction as much as possible, solving collisions when then happen.

The first row or col to be moved is always the closest one to one of the edges, depending on the given direction:

- UP: Top row
- DOWN: Bottom row
- LEFT: Left-most col
- RIGHT: Right-most col

From there, continue moving the next one farther to the direction until reaching the final row or col 
opposite to the direction of the move.

I tried to solve this using State Monad.
