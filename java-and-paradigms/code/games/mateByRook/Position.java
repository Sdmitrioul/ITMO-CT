package mateByRook;

import mateByRook.coordinates.Move;

public interface Position {
    boolean isValid(Move move);
    Cell getCell(int row, int column);
    int[][] getCoordinates();
}
