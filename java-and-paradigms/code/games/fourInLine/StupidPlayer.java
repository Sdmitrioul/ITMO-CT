package fourInLine;

public class StupidPlayer implements Player {
    @Override
    public Move move(final Position position, final Cell cell) {
        for (int column = 0; column < 7; column++) {
            final Move move = new Move(column, cell);
            if (position.isValid(move)) {
                return move;
            }
        }
        throw new IllegalStateException("There's no valid moves");
    }
}
