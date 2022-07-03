package sticks;

import sticks.*;

public class Main {
    public static void main(String[] args) {
        try {
            Player[] players = new Player[3];
            players[0] = new StupidPlayer();
            players[1] = new RandomPlayer();
            players[2] = new RandomPlayer();
            Game game = new Game(false, players);
            Board board = new StickBoard();
            int result = game.play(board);
            if (result == 0){
                System.out.println("TIE");
            } else if (result == -2) {
                System.out.println("Someone lose");
            } else {
                System.out.println("Player " + result + " win!");
            }
            System.out.println("Final board state:\n" + board);
        } catch (InputException e) {
            e.getMessage();
        }
    }
}
