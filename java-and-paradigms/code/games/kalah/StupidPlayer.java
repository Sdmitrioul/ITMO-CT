package kalah;

public class StupidPlayer implements Player {
    @Override
    public Move move(final Position position, final int cell) {
        for (int row = 0; row < 6; row++) {
            final Move move = new Move(row, cell);
            if (position.isValid(move)) {
                return move;
            };

        }
        return new Move(0, cell);
    }
}
