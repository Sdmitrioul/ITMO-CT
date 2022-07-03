package ticTacToe;

import java.util.Scanner;
import java.util.Set;

public class Main {
    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        System.out.println("Input deep of board: ");
        int row = input();
        System.out.println("Input length of board: ");
        int column = input();
        int lineLeength = -1;
        boolean right = false;
        while (!right) {
            System.out.println("Input winning length symbols: ");
            lineLeength = input();
            if (lineLeength <= row && lineLeength <= column){
                right =true;
            } else {
                System.out.println("Input right winning length symbols: ");
            }
        }
        System.out.println("Input rounds count: ");
        int rounds = input();
        Settings settings = new Settings(row, column, lineLeength);
        Player[] players = new Player[6];
        players[0] = new StupidPlayer();
        players[1] = new RandomPlayer();
        for (int i = 2; i < 5; i++) {
            players[i] = new RandomPlayer();
        }
        Championship championship = new Championship(settings, players, rounds);
        System.out.println(championship.getWinner());
        System.out.println(championship.getChampionshipResults());
    }

    private static int input() {
        int number = 0;
        boolean good = false;
        while (!good) {
            if (scanner.hasNextInt()) {
                number = scanner.nextInt();
                if (number > 0) {

                    good = true;
                } else {
                    System.out.println("Введите положительное число!");
                    scanner.next();
                }
            } else {
                if (scanner.hasNext()) {
                    System.out.println("Введите нормально!");
                    scanner.next();
                } else {
                    System.out.println("Поздравляю, вы сломали игру!");
                    System.exit(1);
                }
            }
        }
        return number;
    }
}
