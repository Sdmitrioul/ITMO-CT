package sticks;

public class Game {
    private final boolean log;
    private final Player[] players;
    private int[] playersCount;

    public Game(final boolean log, final Player[] players) {
        this.log = log;
        this.players = players;
        playersCount = new int[players.length];
    }

    public Game(final boolean log, final Player player1, final Player player2) {
        this.log = log;
        this.players = new Player[2];
        players[0] = player1;
        players[1] = player2;
        playersCount = new int[2];
    }

    public int play(Board board) throws InputException {
        boolean oneMore = false;
        while (true) {
            for (int i = 0; i < players.length; i++) {
                if (oneMore) {
                    i--;
                    if (i < 0) {
                        i = players.length - 1;
                    }
                    oneMore = false;
                }
                final int result = move(board, players[i], i);
                if (result != -1 && result != -3) {
                    return result;
                }
                if (result == -3) {
                    oneMore = true;
                }
            }
        }
    }

    private int move(final Board board, final Player player, final int no) throws InputException {
        final Move move = player.move(board.getPosition(), no);
        final Result result = board.makeMove(move);
        log("Player " + no + " move: " + move);
        log("Position:\n" + board);
        boolean oneMoreMove = false;
        if (result == Result.MAKE_SQUARE) {
            playersCount[no]++;
            oneMoreMove = true;
        } else if (result == Result.MAKE_TWO_SQUARES) {
            playersCount[no] += 2;
            oneMoreMove = true;
        } else if (result == Result.LOSE) {
            log("Player " + no + " lose");
            return -2;
        }
        if (!board.getPosition().haveAnyValidMoves()) {
            log("It was last move");
            int winner = findWinner();
            if (winner == -2) {
                log("Draw");
                return 0;
            } else {
                log("Player " + winner + " won");
                return winner;
            }
        }
        if (oneMoreMove) {
            return -3;
        }
        return -1;
    }

    private int findWinner() {
        int max = 0;
        int no = 0;
        boolean draw = false;
        for (int i = 0; i < players.length; i++) {
            if (max < playersCount[i]) {
                no = i;
                max = playersCount[i];
                draw = false;
            } else if (max == playersCount[i]) {
                draw = true;
            }
        }
        if (draw) {
            return -2;
        } else {
            return no + 1;
        }
    }

    private void log(final String message) {
        if (log) {
            System.out.println(message);
        }
    }
}
