package mateByRook.player;

import mateByRook.Colour;
import mateByRook.InputException;
import mateByRook.Position;
import mateByRook.coordinates.Move;

public interface Player {
    Move move(Position position, Colour colour) throws InputException;
}
