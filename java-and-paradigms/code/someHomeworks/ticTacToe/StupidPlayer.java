package ticTacToe;

public class StupidPlayer implements Player {
    @Override
    public Move move(final Position position, final Cell cell, final Settings settings) {
        for (int row = 0; row < settings.getRow(); row++) {
            for (int column = 0; column < settings.getColumn(); column++) {
                final Move move = new Move(row, column, cell);
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        throw new IllegalStateException("There's no valid moves");
    }
}
