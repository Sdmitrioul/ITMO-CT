package sticks;

public interface Board {
    Result makeMove(Move move);
    Position getPosition();
}
