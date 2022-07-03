package mateByRook;

import mateByRook.coordinates.Move;

public interface Board {
    Result makeMove(Move move);
    Colour getColour();
    Position getPosition();
}
