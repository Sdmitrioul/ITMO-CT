package mateByRook;

import mateByRook.*;
import mateByRook.coordinates.FirstPosition;
import mateByRook.player.HumanPlayer;
import mateByRook.player.Winner;

import java.util.Map;

public class Main {
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
    // First is coordinates of WHITE_KING, second - WHITE_ROOK, third - BLACK_KING (coordinatees are in format _Character_Integer_ (Column, Row))
    public static void main(String[] args) throws InputException {
        if (args.length != 3 || !rightString(args[0]) || !rightString(args[1]) || !rightString(args[2])) {
            try {
                throw new InputException("Coodinates have wrong format");
            } catch (InputException e) {
                e.getMessage();
            }
        }
        FirstPosition[] firstPositions = new FirstPosition[3];
        /*firstPositions[0] = new FirstPosition(Cell.KING_WHITE, Integer.parseInt(args[0].substring(1)) - 1, VOCABULARY.get(args[0].charAt(0)));
        firstPositions[1] = new FirstPosition(Cell.ROOK, Integer.parseInt(args[1].substring(1)) - 1, VOCABULARY.get(args[1].charAt(0)));
        firstPositions[2] = new FirstPosition(Cell.KING_BLACK, Integer.parseInt(args[2].substring(1)) - 1, VOCABULARY.get(args[2].charAt(0)));*/
        firstPositions[0] = new FirstPosition(Cell.KING_WHITE, 3, 3);
        firstPositions[1] = new FirstPosition(Cell.ROOK, 4, 3);
        firstPositions[2] = new FirstPosition(Cell.KING_BLACK, 1, 3);
        try {
            MateByRook mateByRook = new MateByRook(firstPositions);
            System.out.println("Player " + mateByRook.play() + " win");
        } catch (InputException e) {
            e.getMessage();
        }
    }

    private static boolean rightString(String string){
        return string.length() == 2 && VOCABULARY.containsKey(string.charAt(0)) && Character.isDigit(string.charAt(1));
    }
}
