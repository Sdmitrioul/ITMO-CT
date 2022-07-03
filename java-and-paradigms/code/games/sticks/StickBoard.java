package sticks;

import java.util.Arrays;
import java.util.Map;

public class StickBoard implements Board, Position {
    final static Map<Cell, Character> SYMBOLS = Map.of(
            Cell.STICK_HORIZONTAL, '-',
            Cell.STICK_VERTICAL, '|',
            Cell.EMPTY, '*'
    );
    final static Map<Integer, Character> VOCABULARY = Map.of(
            0, 'a',
            1, 'b',
            2, 'c',
            3, 'd',
            4, 'e',
            5, 'f',
            6, 'g',
            7, 'h'
    );

    private Cell[][] cellsVertical;
    private Cell[][] cellsHorizontal;
    private int emptyCells;

    public StickBoard() {
        this.cellsVertical = new Cell[6][7];
        this.cellsHorizontal = new Cell[7][6];
        emptyCells = 84;
        for (Cell[] row: cellsVertical) {
            Arrays.fill(row, Cell.EMPTY);
        }
        for (Cell[] row: cellsHorizontal) {
            Arrays.fill(row, Cell.EMPTY);
        }
    }

    @Override
    public boolean haveAnyValidMoves() {
        return emptyCells != 0;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }

        emptyCells--;

        if (move.isHorizontal()) {
            cellsHorizontal[move.getRow()][move.getColumn()] = Cell.STICK_HORIZONTAL;
        } else {
            cellsVertical[move.getRow()][move.getColumn()] = Cell.STICK_VERTICAL;
        }
        int countSquares = madeSquares(move);
        if (countSquares == 2) {
            return Result.MAKE_TWO_SQUARES;
        } else if (countSquares == 1) {
            return Result.MAKE_SQUARE;
        }
        return Result.NONE;
    }

    /* 1a2b3c4d5e6f7
      1 - - - - - - 0 0
      a| | | | | | |1 0
      2 * * * * * * 2 1
      b* * * * * * *3 1
      3 - - - - - - 4 2
      c| | | | | | |5 2
      4 - - - - - - 6 3
      d| | | | | | |7 3
      5 - - - - - - 8 4
      e| | | | | | |9 4
      6 - - - - - - 10 5
      f| | | | | | |11 5
      7 - - - - - - 12 6
       0123456789012
       0011223344556*/

    private int madeSquares(Move move) {
        int counter = 0;
        if (move.isHorizontal()) {
            //UP Square
            if (move.getRow() > 0 && cellsHorizontal[move.getRow() - 1][move.getColumn()] != Cell.EMPTY
                    && cellsVertical[move.getRow() - 1][move.getColumn()] != Cell.EMPTY
                    && cellsVertical[move.getRow() - 1][move.getColumn() + 1] != Cell.EMPTY) {
                counter++;
            }
            //DOWN Square
            if (move.getRow() < 6 && cellsHorizontal[move.getRow() + 1][move.getColumn()] != Cell.EMPTY
                    && cellsVertical[move.getRow()][move.getColumn()] != Cell.EMPTY
                    && cellsVertical[move.getRow()][move.getColumn() + 1] != Cell.EMPTY) {
                counter++;
            }
        } else {
            //LEFT Square
            if (move.getColumn() > 0 && cellsVertical[move.getRow()][move.getColumn() - 1] != Cell.EMPTY
                    && cellsHorizontal[move.getRow()][move.getColumn() - 1] != Cell.EMPTY
                    && cellsHorizontal[move.getRow() + 1][move.getColumn() - 1] != Cell.EMPTY) {
                counter++;
            }
            //Right Square
            if (move.getColumn() < 6 && cellsVertical[move.getRow()][move.getColumn() + 1] != Cell.EMPTY
                    && cellsHorizontal[move.getRow()][move.getColumn()] != Cell.EMPTY
                    && cellsHorizontal[move.getRow() + 1][move.getColumn()] != Cell.EMPTY) {
                counter++;
            }
        }
        return counter;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public boolean isValid(Move move) {
        if (move.isHorizontal()) {
            return move.getRow() > -1 && move.getRow() < 7 && move.getColumn() > -1 && move.getColumn() < 6 && cellsHorizontal[move.getRow()][move.getColumn()] == Cell.EMPTY;
        } else {
            return move.getRow() > -1 && move.getRow() < 6 && move.getColumn() > -1 && move.getColumn() < 7 && cellsVertical[move.getRow()][move.getColumn()] == Cell.EMPTY;
        }
    }

    @Override
    public Cell getCell(int row, int column, boolean isHorizontal) {
        if (isHorizontal) {
            return cellsHorizontal[row][column];
        } else {
            return cellsVertical[row][column];
        }
    }

    /* 1a2b3c4d5e6f7
      1 - - - - - - 0 0
      a| | | | | | |1 0
      2 * * * * * * 2 1
      b* * * * * * *3 1
      3 - - - - - - 4 2
      c| | | | | | |5 2
      4 - - - - - - 6 3
      d| | | | | | |7 3
      5 - - - - - - 8 4
      e| | | | | | |9 4
      6 - - - - - - 10 5
      f| | | | | | |11 5
      7 - - - - - - 12 6
       0123456789012
       0011223344556*/

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" 1a2b3c4d5e6f7");
        int counterHor = 0;
        int counterVer = 0;
        for (int r = 0; r < 13; r++) {
            sb.append("\n");
            if (r % 2 == 0) {
                counterHor++;
                sb.append(counterHor).append(" ");
                for (int i = 0; i < 6; i++) {
                    sb.append(SYMBOLS.get(cellsHorizontal[counterHor - 1][i])).append(" ");
                }
            } else {
                sb.append(VOCABULARY.get(counterVer));
                counterVer++;
                for (int i = 0; i < 7; i++) {
                    sb.append(SYMBOLS.get(cellsVertical[counterHor - 1][i])).append(" ");
                }
            }
        }
        return sb.toString();
    }
}
