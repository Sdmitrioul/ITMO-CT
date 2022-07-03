package fourInLine;

import java.util.Scanner;

public class Main {
    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) throws InputExeption {
        System.out.println("Input rounds count: ");
        int rounds = input();
        Player[] players = new Player[2];
        players[0] = new StupidPlayer();
        players[1] = new RandomPlayer();
        /*for (int i = 2; i <= 5; i++) {
            players[i] = new RandomPlayer();
        }*/
        Championship championship = new Championship(players, rounds);
        System.out.println(championship.getWinner());
        System.out.println(championship.getChampionshipResults());
    }

    private static int input() throws InputExeption {
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
                    throw new InputExeption("Вы сломали игру!");
                }
            }
        }
        return number;
    }
}
