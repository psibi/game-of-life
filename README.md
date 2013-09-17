#A haskell solution for Conway's GAME OF LIFE

## Internals

    type Location = (Int, Int)

Represents the Location of the Cell.

    data Cell = Alive | Dead

Represents whether the Cell is Alive or Dead.

    type World = [(Location, Cell)]

Represents the entire world with it's state. A sample world's location
is represented like this:

    +---+---+---+---+
    |0,0|0,1|0,2|0,3|
    |   |   |   |   |
    +---+---+---+---+
    |1,0|1,1|1,2|1,3|
    |   |   |   |   |
    +---+---+---+---+
    |2,0|2,1|2,2|2,3|
    |   |   |   |   |
    +---+---+---+---+
    |3,0|3,1|3,2|3,3|
    |   |   |   |   |
    +---+---+---+---+

The world is assumed to be in an rectangular grid.
