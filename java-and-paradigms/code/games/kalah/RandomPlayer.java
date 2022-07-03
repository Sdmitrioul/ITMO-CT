package kalah;

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
            int row = random.nextInt(6);
            final Move move = new Move(row, cell);
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
