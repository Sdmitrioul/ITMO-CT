package fourInLine;

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
    public Move move(final Position position, final Cell cell) throws InputExeption {
        while (true) {
            printStream.println("Position");
            printStream.println(position);
            printStream.println(cell + "'s move");
            printStream.println("Enter column");
            int col = -1;
            boolean good = false;
            while (!good) {
                if (scanner.hasNextInt()) {
                    col = scanner.nextInt();
                    good = true;
                } else {
                    if (scanner.hasNext()) {
                        printStream.println("Введите нормально!");
                        scanner.next();
                        good = false;
                    } else {
                        throw new InputExeption("Dы сломали игру!");
                    }
                }
            }
            final Move move = new Move(col, cell);
            if (position.isValid(move)) {
                return move;
            }
            printStream.println("Move " + move + " is invalid");
        }
    }
}
