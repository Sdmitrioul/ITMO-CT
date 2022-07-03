package mateByRook.player;

import mateByRook.Colour;
import mateByRook.InputException;
import mateByRook.Position;
import mateByRook.coordinates.Move;

import java.io.PrintStream;
import java.util.Map;
import java.util.Scanner;

public class HumanPlayer implements Player {
    private final Scanner scanner;
    private final PrintStream printStream;
    private final static Map<Character, Integer> VOCABULARY = Map.of(
            'a', 0,
            'b', 1,
            'c', 2,
            'd', 3,
            'e', 4,
            'f', 5,
            'g', 6,
            'h', 7
    );

    public HumanPlayer(Scanner scanner, PrintStream printStream) {
        this.scanner = scanner;
        this.printStream = printStream;
    }

    public HumanPlayer() {
        this(new Scanner(System.in), System.out);
    }

    @Override
    public Move move(final Position position, final Colour colour) throws InputException {
        while (true) {
            printStream.println("Position");
            printStream.println(position);
            printStream.println(colour + "'s move");
            printStream.println("Enter colour coordinates from (in format(column, row) b2)");
            String from = "";
            boolean good = false;
            while (!good) {
                if (scanner.hasNextLine()) {
                    from = scanner.nextLine();
                    if (rightString(from)) {
                        good = true;
                    } else {
                        printStream.println("Введите нормально!");
                    }
                } else {
                    if (scanner.hasNext()) {
                        printStream.println("Введите нормально!");
                        scanner.next();
                        good = false;
                    } else {
                        throw new InputException("Вы сломали игру!");
                    }
                }
            }
            printStream.println("Enter colour coordinates to (in format(column, row) b2)");
            String to = "";
            good = false;
            while (!good) {
                if (scanner.hasNextLine()) {
                    to = scanner.nextLine();
                    if (rightString(to)) {
                        good = true;
                    } else {
                        printStream.println("Введите нормально!");
                    }
                } else {
                    if (scanner.hasNext()) {
                        printStream.println("Введите нормально!");
                        scanner.next();
                        good = false;
                    } else {
                        throw new InputException("Вы сломали игру!");
                    }
                }
            }
            final Move move = new Move(Integer.parseInt(from.substring(1)) - 1, VOCABULARY.get(from.charAt(0)), Integer.parseInt(to.substring(1)) - 1, VOCABULARY.get(to.charAt(0)));
            if (position.isValid(move)) {
                return move;
            }
            printStream.println("Move " + move + " is invalid");
        }
    }

    private boolean rightString(String string){
        return string.length() == 2 && VOCABULARY.containsKey(string.charAt(0)) && Character.isDigit(string.charAt(1));
    }
}
