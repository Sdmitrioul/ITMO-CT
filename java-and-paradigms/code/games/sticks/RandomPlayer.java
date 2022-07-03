package sticks;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random;

    public RandomPlayer(final Random random) {
        this.random = random;
    }

    public RandomPlayer() {
        this(new Random());
    }

    @Override
    public Move move(final Position position, final int cell) {
        while (true) {
            int row = random.nextInt(7);
            int column = random.nextInt(7);
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
}
