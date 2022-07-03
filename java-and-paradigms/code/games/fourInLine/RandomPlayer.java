package fourInLine;

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
    public Move move(final Position position, final Cell cell) {
        while (true) {
            int column = random.nextInt(7);
            final Move move = new Move(column, cell);
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
