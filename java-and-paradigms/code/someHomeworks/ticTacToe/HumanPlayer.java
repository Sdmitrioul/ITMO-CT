package ticTacToe;

import java.io.PrintStream;
import java.util.Scanner;

public class HumanPlayer implements Player {
    private final Scanner scanner;
    private final PrintStream printStream;

    public HumanPlayer(Scanner scanner, PrintStream printStream) {
        this.scanner = scanner;
        this.printStream = printStream;
    }

    public HumanPlayer() {
        this(new Scanner(System.in), System.out);
    }

    @Override
    public Move move(final Position position, final Cell cell, final Settings settings) {
        while (true) {
            printStream.println("Position");
            printStream.println(position);
            printStream.println(cell + "'s move");
            printStream.println("Enter row and column");
            int col = -1;
            int row = -1;
            boolean good = false;
            while (!good) {
                if (scanner.hasNextInt()) {
                    row = scanner.nextInt();
                    good = true;
                } else {
                    if (scanner.hasNext()) {
                        printStream.println("Введите нормально!");
                        scanner.next();
                        good = false;
                    } else {
                        printStream.println("Поздравляю, вы сломали игру!");
                        System.exit(1);
                    }
                }
                if (scanner.hasNextInt()) {
                    col = scanner.nextInt();
                    good = true;
                } else {
                    if (scanner.hasNext()) {
                        printStream.println("Введите нормально!");
                        scanner.next();
                        good = false;
                    } else {
                        printStream.println("Поздравляю, вы сломали игру!");
                        System.exit(1);
                    }
                }
            }
            final Move move = new Move(row, col, cell);
            if (position.isValid(move)) {
                return move;
            }
            printStream.println("Move " + move + " is invalid");
        }
    }
}
