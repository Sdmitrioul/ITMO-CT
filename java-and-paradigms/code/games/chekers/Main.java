package chekers;

public class Main {
    public static void main(String[] args) {
        Game game = new Game(false, new HumanPlayer(), new HumanPlayer());
        Board board = new CheckersBoard();
        game.play(board);
    }
}
