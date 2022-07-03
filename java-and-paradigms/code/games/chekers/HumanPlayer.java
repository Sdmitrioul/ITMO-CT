package chekers;

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
    public Move move(final Position position, final Cell cell) {
        while (true) {
            printStream.println("Position");
            printStream.println(position);
            printStream.println(cell + "'s move");
            printStream.println("Enter row and column from");
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
            int row2 = -1;
            int col2 = -1;
            good = false;
            printStream.println("Enter row and column to move");
            while (!good) {
                if (scanner.hasNextInt()) {
                    row2 = scanner.nextInt();
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
                    col2 = scanner.nextInt();
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

            final Move move = new Move(row, col, row2, col2, cell);
            if (position.isValid(move)) {
                return move;
            }
            printStream.println("Move " + move + " is invalid");
        }
    }
}
