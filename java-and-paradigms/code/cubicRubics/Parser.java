package cubicRubics;

import java.util.Map;
import java.util.NoSuchElementException;

public class Parser {
    private Command[] commands;

    private static final Map<Character, Edge> EDGE_CHARACTER_MAP = Map.of(
            'B', Edge.B,
            'F', Edge.F,
            'D', Edge.D,
            'U', Edge.U,
            'R', Edge.R,
            'L', Edge.L
    );

    public Parser(String string) {
        int count = 0;
        for (int i = 0; i < string.length(); i++) {
            if (string.charAt(i) != '`') {
                count++;
            }
        }
        commands = new Command[count];
        count = 0;
        for (int i = 0; i < string.length(); i++) {
            if (i != 0 && string.charAt(i) == '`') {
                commands[count - 1].setDirection(false);
            } else {
                if (EDGE_CHARACTER_MAP.get(string.charAt(i)) == null) {
                    throw new NoSuchElementException();
                } else {
                    commands[count++] = new Command(EDGE_CHARACTER_MAP.get(string.charAt(i)));
                }
            }
        }
    }

    public Command[] getCommands() {
        return commands;
    }
}
