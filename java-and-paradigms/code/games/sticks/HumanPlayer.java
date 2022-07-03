package sticks;

import java.io.PrintStream;
import java.rmi.MarshalledObject;
import java.util.Map;
import java.util.Scanner;

public class HumanPlayer implements Player {
    private final Scanner scanner;
    private final PrintStream printStream;
    final static Map<Character, Integer> VOCABULARY = Map.of(
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
    public Move move(final Position position, int cell) throws InputException {
        while (true) {
            printStream.println("Position");
            printStream.println(position);
            printStream.println(cell + 1 + " player's move");
            printStream.println("Enter cell coordinates (in format(row, column) b2 or 2b)");
            String to = "";
            boolean good = false;
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
            Move move;
            if (Character.isDigit(to.charAt(0))) {
                move = new Move(Integer.parseInt(to.substring(0, 1)) - 1, VOCABULARY.get(to.charAt(1)), true);
            } else {
                move = new Move(VOCABULARY.get(to.charAt(0)), Integer.parseInt(to.substring(1, 2)) - 1, false);
            }
            if (position.isValid(move)) {
                return move;
            }
            printStream.println("Move " + move + " is invalid");
        }
    }

    private boolean rightString(String string){
        return string.length() == 2 && ((VOCABULARY.containsKey(string.charAt(0)) && Character.isDigit(string.charAt(1)))
                || (VOCABULARY.containsKey(string.charAt(1)) && Character.isDigit(string.charAt(0))));
    }
}
