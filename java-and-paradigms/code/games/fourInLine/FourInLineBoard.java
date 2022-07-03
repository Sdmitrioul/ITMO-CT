package fourInLine;

import java.util.Arrays;
import java.util.Map;

public class FourInLineBoard implements Board, Position {
    final static Map<Cell, Character> SYMBOLS = Map.of(
            Cell.BLACK, 'B',
            Cell.WHITE, 'W',
            Cell.EMPTY, '*'
    );

    private final Cell[][] cells;
    private Cell turn;
    private int emptyCells;

    public FourInLineBoard() {
        this.cells = new Cell[6][7];
        emptyCells = 42;
        for (Cell[] row: cells) {
            Arrays.fill(row, Cell.EMPTY);
        }
        turn = Cell.BLACK;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        int row = findLowestInRaw(move.getColumn());
        cells[row][move.getColumn()] = move.getValue();
        emptyCells--;
        if (checkWinningMove(row, move.getColumn())){
            return Result.WIN;
        }
        if (emptyCells == 0) {
            return Result.DRAW;
        }
        turn = turn == Cell.BLACK ? Cell.WHITE : Cell.BLACK;
        return Result.UNKNOWN;
    }

    private int findLowestInRaw(int column) {
        for (int i = 5; i >= 0; i--) {
            if (cells[i][column] == Cell.EMPTY) {
                return i;
            }
        }
        return 0;
    }

    private boolean checkWinningMove(int row, int column) {
        return checkInColumn(row, column) || checkInRaw(row, column);
    }

    private boolean checkInRaw(int row, int column) {
        int count = 1;
        int tmp = column;
        while (tmp > 0 && cells[row][tmp - 1] == turn) {
            count++;
            tmp--;
        }
        tmp = column;
        while (tmp < 6 && cells[row][tmp + 1] == turn) {
            count++;
            tmp++;
        }
        return count >= 4;
    }

    private boolean checkInColumn(int row, int column) {
        int count = 0;
        int tmp = row;
        while (tmp <= 5 && cells[tmp][column] == turn) {
            count++;
            tmp++;
        }
        return count >= 4;
    }

    @Override
    public Cell getCell() {
        return turn;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public boolean isValid(Move move) {
        return 0 <= move.getColumn() && move.getColumn() < 7 && hasEmptyInColumn(move.getColumn()) && turn == move.getValue();
    }

    private boolean hasEmptyInColumn(int column) {
        for (int i = 5; i >= 0; i--) {
            if (cells[i][column] == Cell.EMPTY) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Cell getCell(int row, int column) {
        return cells[row][column];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 7; i++) {
            sb.append(i);
        }

        for (int r = 0; r < 6; r++) {
            sb.append("\n");
            for (int c = 0; c < 7; c++) {
                sb.append(SYMBOLS.get(cells[r][c]));
            }
        }
        return sb.toString();
    }
}
