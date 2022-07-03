package reversi;

import java.util.Arrays;
import java.util.Map;

public class ReversiBoard implements Board, Position {
    final static Map<Cell, Character> SYMBOLS = Map.of(
            Cell.B, 'B',
            Cell.W, 'W',
            Cell.E, '*'
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

    private Cell[][] cells;
    private Cell turn;
    private int emptyCells;
    private int counterBlack;
    private int counterWhite;

    public ReversiBoard() {
        this.cells = new Cell[8][8];
        emptyCells = 60;
        for (Cell[] row: cells) {
            Arrays.fill(row, Cell.E);
        }
        cells[3][3] = Cell.W;
        cells[4][4] = Cell.W;
        cells[3][4] = Cell.B;
        cells[4][3] = Cell.B;
        counterBlack = 2;
        counterWhite = 2;
        turn = Cell.B;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        paint(move);

        //System.out.println("Bl : " + counterBlack + "; Wh : " + counterWhite + "; Empty : " + emptyCells);

        if (emptyCells == 0) {
            if (counterBlack > counterWhite) {
                if (turn == Cell.B) {
                    return Result.WIN;
                } else {
                    return Result.LOSE;
                }
            } else if (counterWhite > counterBlack){
                if (turn == Cell.W) {
                    return Result.WIN;
                } else {
                    return Result.LOSE;
                }
            } else {
                return Result.DRAW;
            }
        }
        turn = turn == Cell.B ? Cell.W : Cell.B;
        if (!opponentHaveValidMoves()) {
            return Result.WIN;
        }
        return Result.UNKNOWN;
    }

    private void paint(Move move) {
        cells[move.getRow()][move.getColumn()] = turn;
        int counter = 0;
        emptyCells--;
        for (int i = -1; i < 2; i++) {
            for (int j = -1; j < 2; j++) {
                if (onVector(move, i, j)) {
                    int row = move.getRow() + i;
                    int column = move.getColumn() + j;
                    while (cells[row][column] != turn) {
                        cells[row][column] = turn;
                        counter++;
                        row += i;
                        column += j;
                    }
                }
            }
        }
        if (turn == Cell.B) {
            counterBlack += counter + 1;
            counterWhite -= counter;
        } else {
            counterWhite += counter + 1;
            counterBlack -= counter;
        }
    }

    private boolean opponentHaveValidMoves() {
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++) {
                Move move = new Move(i, j, turn);
                if (isValid(move)) {
                    return true;
                }
            }
        }
        return false;
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
        if (cells[move.getRow()][move.getColumn()] != Cell.E) {
            return false;
        }
        boolean checker = false;
        for (int i = -1; i < 2; i++) {
            for (int j = -1; j < 2; j++) {
                //System.out.println(i + " " + j + " " + onVector(move, i, j));
                checker = checker || onVector(move, i, j);
            }
        }
        return cells[move.getRow()][move.getColumn()] == Cell.E && checker;
    }

    private boolean onVector(Move move, int dx, int dy) {
        Cell opposite = move.getValue() == Cell.B ? Cell.W : Cell.B;
        int row = move.getRow();
        int column = move.getColumn();
        while (row + dx < 8 && column + dy < 8 && row + dx > -1 && column + dy > -1 && cells[row + dx][column + dy] == opposite) {
            row += dx;
            column += dy;
        }
        row += dx;
        column +=dy;
        if ((row != move.getRow() || column != move.getColumn()) && row < 8 && column < 8 && row > -1 && column > -1 &&
                (row - move.getRow() != dx || column - move.getColumn() != dy) && cells[row][column] == move.getValue()) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public Cell getCell(int row, int column) {
        return cells[row][column];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" 12345678");
        for (int r = 0; r < 8; r++) {
            sb.append("\n");
            sb.append(VOCABULARY.get(r));
            for (int c = 0; c < 8; c++) {
                sb.append(SYMBOLS.get(cells[r][c]));
            }
        }
        return sb.toString();
    }
}
