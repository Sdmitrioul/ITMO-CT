package kalah;

public interface Board {
    Result makeMove(Move move);
    int getCell();
    Position getPosition();
}
