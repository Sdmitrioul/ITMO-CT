package kalah;

public interface Position {
    boolean isValid(Move move);
    int getCell(int row, int column);
}
