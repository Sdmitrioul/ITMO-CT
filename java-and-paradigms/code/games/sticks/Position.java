package sticks;

public interface Position {
    boolean isValid(Move move);
    boolean haveAnyValidMoves();
    Cell getCell(int row, int column, boolean isHorizontal);
}
