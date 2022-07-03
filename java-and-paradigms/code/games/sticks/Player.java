package sticks;

public interface Player {
    Move move(Position position, int cell) throws InputException;
}
