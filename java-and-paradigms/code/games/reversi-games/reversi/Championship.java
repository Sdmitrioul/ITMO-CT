package reversi;

public class Championship implements AbstractChampionship {
    private final Player[] players;
    private final int rounds;
    private int[][] results;

    public Championship(final Player[] players, final int rounds) throws InputException {
        this.players = players;
        this.rounds = rounds;
        results = new int[rounds + 1][players.length];
        int round = 0;
        do {
            System.out.println("Round : " + (round + 1));
            round++;
            for (int i = 0; i < players.length; i++) {
                for (int j = i + 1; j < players.length; j++) {
                    Game game = new Game(false, players[i],  players[j]);
                    Board board = new ReversiBoard();
                    System.out.println("Player " + (i + 1) + " vs " + "player " + (j + 1));
                    int result = game.play(board);
                    if (result == 1) {
                        results[round][i] = results[round - 1][i] + 3;
                        System.out.println("Player " + (i + 1) + ": WIN");
                    } else {
                        if (result == 2) {
                            results[round][j] = results[round - 1][j] + 3;
                            System.out.println("Player " + (j + 1) + ": WIN");
                        } else {
                            results[round][i] = results[round - 1][i] + 1;
                            results[round][j] = results[round - 1][j] + 1;
                            System.out.println("TIE");
                        }
                    }
                    System.out.println("Final board state:\n" + board.toString());
                }
            }
            if (round != rounds) {
                System.out.println(getRoundResult(round));
            }
        } while (round < rounds);
    }

    @Override
    public int getColRounds() {
        return rounds;
    }

    @Override
    public String getRoundResult(int round) {
        StringBuilder sb = new StringBuilder("Result of " + round + " is: ");
        for (int i = 0; i < players.length; i++) {
            sb.append("\n").append("Player ").append(i + 1).append(" score is: ").append(results[round][i]);
        }
        return sb.toString();
    }

    @Override
    public String getChampionshipResults() {
        StringBuilder sb = new StringBuilder("Result of championship is: ");
        for (int i = 0; i < players.length; i++) {
            sb.append("\n").append("Player ").append(i + 1).append(" score is: ").append(results[rounds][i]);
        }
        return sb.toString();
    }

    @Override
    public int getWinnerNumber() {
        int maxvalue = -1;
        int numberwithmax = 0;
        boolean win = true;
        for (int i = 0; i < players.length; i++) {
            if (maxvalue < results[rounds][i]) {
                maxvalue = results[rounds][i];
                numberwithmax = i;
                win = true;
            } else {
                if (maxvalue == results[rounds][i]) {
                    win = false;
                }
            }
        }
        if (win) {
            return numberwithmax + 1;
        }
        return -1;
    }

    @Override
    public String getWinner() {
        int win = getWinnerNumber();
        if (win == -1) {
            return "There is no winner's";
        }
        return "Player " + win + " win this championship";
    }
}
