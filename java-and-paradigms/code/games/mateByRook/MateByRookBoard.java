package mateByRook;

import mateByRook.coordinates.FirstPosition;
import mateByRook.coordinates.Move;

import java.util.Arrays;
import java.util.Map;

public class MateByRookBoard implements Position, Board {
    final static Map<Cell, Character> SYMBOLS = Map.of(
            Cell.KING_BLACK, 'B',
            Cell.KING_WHITE, 'W',
            Cell.EMPTY, '|',
            Cell.ROOK, 'R'
    );

    private Cell[][] cells;
    private Colour turn;
    private int[][] coordinates;
    /*1. White king
      2. White rook
      3. Black king
    */

    public MateByRookBoard(FirstPosition[] firstPositions) {
        this.cells = new Cell[8][8];
        this.coordinates = new int[3][2];
        for (Cell[] row: cells) {
            Arrays.fill(row, Cell.EMPTY);
        }
        for (int i = 0; i < 3; i++) {
            cells[firstPositions[i].getRow()][firstPositions[i].getColumn()] = firstPositions[i].getCell();
            coordinates[i][0] = firstPositions[i].getRow();
            coordinates[i][1] = firstPositions[i].getColumn();
        }
        turn = Colour.WHITE;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        Cell cell = cells[move.getFromRow()][move.getFromColumn()];
        cells[move.getFromRow()][move.getFromColumn()] = Cell.EMPTY;
        cells[move.getToRow()][move.getToColumn()] = cell;
        if (cell == Cell.KING_BLACK) {
            coordinates[2][0] = move.getToRow();
            coordinates[2][1] = move.getToColumn();
        } else if (cell == Cell.KING_WHITE) {
            coordinates[0][0] = move.getToRow();
            coordinates[0][1] = move.getToColumn();
        } else {
            coordinates[1][0] = move.getToRow();
            coordinates[1][1] = move.getToColumn();
        }
        turn = turn == Colour.WHITE ? Colour.BLACK : Colour.WHITE;
        if (turn == Colour.BLACK && !blackKingHaveValidMoves()) {
            return Result.WIN;
        }
        return Result.UNKNOWN;
    }

    @Override
    public int[][] getCoordinates() {
        return coordinates;
    }

    private boolean blackKingHaveValidMoves() {
        int row = coordinates[2][0];
        int column = coordinates[2][1];
        for (int i = -1; i < 2; i++) {
            for (int j = -1; j < 2; j++ ) {
                Move move = new Move(row, column, row + i, column + j);
                if (isValid(move))
                    return true;
            }
        }
        return false;
    }

    @Override
    public Colour getColour() {
        return turn;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public boolean isValid(Move move) {
        int drow = move.getFromColumn() - move.getToColumn();
        int dcolumn = move.getFromRow() - move.getToRow();
        if (turn == Colour.WHITE)
            return true;
        if (move.getToRow() >= 8 || move.getToRow() < 0 || move.getToColumn() >= 8 || move.getToColumn() < 0
                || move.getFromRow() >= 8 || move.getFromRow() < 0 || move.getFromColumn() >= 8 || move.getFromColumn() < 0 || (drow == 0 && dcolumn == 0))
            return false;
        if (turn == Colour.BLACK && cells[move.getFromRow()][move.getFromColumn()] == Cell.KING_BLACK && Math.abs(dcolumn) <= 1 && Math.abs(drow) <= 1
                && !(move.getToRow() == coordinates[1][0] || move.getToColumn() == coordinates[1][1]
                || (Math.abs(move.getToRow() - coordinates[0][0]) <= 1 && Math.abs(move.getToColumn() - coordinates[0][1]) <= 1))) {
            return true;
        } else if (turn == Colour.WHITE && cells[move.getFromRow()][move.getFromColumn()] == Cell.KING_WHITE && Math.abs(dcolumn) <= 1 && Math.abs(drow) <= 1
                && !(Math.abs(move.getToRow() - coordinates[2][0]) <= 1 && Math.abs(move.getToColumn() - coordinates[2][1]) <= 1)) {
            return true;
        } else if (turn == Colour.WHITE && cells[move.getFromRow()][move.getFromColumn()] == Cell.ROOK && ((Math.abs(dcolumn) == 0 && Math.abs(drow) <= 7) || (Math.abs(dcolumn) <= 7 && Math.abs(drow) == 0))
                && (!(Math.abs(move.getToRow() - coordinates[2][0]) <= 1 && Math.abs(move.getToColumn() - coordinates[2][1]) <= 1)
                || ((Math.abs(move.getToRow() - coordinates[2][0]) <= 1 && Math.abs(move.getToColumn() - coordinates[2][1]) <= 1) && (Math.abs(move.getToRow() - coordinates[0][0]) <= 1 && Math.abs(move.getToColumn() - coordinates[0][1]) <= 1)))) {
            return true;
        }
        return false;
    }

    @Override
    public Cell getCell(int row, int column) {
        return cells[row][column];
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(" abcdefgh");
        for (int i = 0; i < 8; i++) {
            sb.append('\n');
            sb.append(i + 1);
            for (int j = 0; j < 8; j++) {
                sb.append(SYMBOLS.get(cells[i][j]));
            }
        }
        return sb.toString();
    }
}
