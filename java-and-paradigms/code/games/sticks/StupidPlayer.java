package sticks;

public class StupidPlayer implements Player {
    @Override
    public Move move(Position position, int cell) throws InputException {
        for (int row = 0; row < 7; row++) {
            for (int column = 0; column < 7; column++) {
                if (row != 6 && column != 6) {
                    Move move = new Move(row, column, true);
                    if (position.isValid(move)) {
                        return move;
                    }
                    move = new Move(row, column, false);
                    if (position.isValid(move)) {
                        return move;
                    }
                } else if (row == 6 && column != 6) {
                    Move move = new Move(row, column, true);
                    if (position.isValid(move)) {
                        return move;
                    }
                } else if (column == 6 && row != 6) {
                    Move move = new Move(row, column, false);
                    if (position.isValid(move)) {
                        return move;
                    }
                }
            }
        }
        throw new IllegalStateException("There's no valid moves");
    }
}
