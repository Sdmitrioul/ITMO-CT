package reversi;

public class StupidPlayer implements Player {
    @Override
    public Move move(Position position, Cell cell) throws InputException {
        for (int row = 0; row < 8; row++) {
            for (int column = 0; column < 8; column++) {
                final Move move = new Move(row, column, cell);
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        throw new IllegalStateException("There's no valid moves");
    }
}
