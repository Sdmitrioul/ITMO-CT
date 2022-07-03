package chekers;

import java.util.Map;

public class CheckersBoard implements Board, Position{
    final static Map<Cell, String> SYMBOLS = Map.of(
            Cell.Bl, "B",
            Cell.Wh, "W",
            Cell.Un, "#",
            Cell.Eb, "*"
    );

    private Cell[][] cells;
    private Cell turn;
    private int countBlack;
    private int countWhite;
    private int count = 120;

    public CheckersBoard() {
        this.cells = new Cell[8][8];
        countBlack = 12;
        countWhite = 12;
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++) {
                cells[i][j] = (i + j) % 2 == 0 ? Cell.Un : Cell.Eb;
                if (cells[i][j] == Cell.Eb && (i < 3 || i > 4)) {
                    cells[i][j] = i < 3 ? Cell.Wh : Cell.Bl;
                }
            }
        }
        turn = Cell.Wh;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }

        cells[move.getTorow()][move.getTocolumn()] = move.getValue();
        cells[move.getFromrow()][move.getFromcolumn()] = Cell.Eb;
        count--;
        if (eteded(move.getFromrow(), move.getFromcolumn(), move.getTorow(), move.getTocolumn())) {
            if (turn == Cell.Wh) {
                countBlack--;
            } else {
                countWhite--;
            }
            clean(move.getFromrow(), move.getFromcolumn(), move.getTorow(), move.getTocolumn());
        }
        if (countWhite == 0) {
            return Result.WIN;
        }
        if (countBlack == 0) {
            return Result.WIN;
        }
        if (count == 0) {
            return Result.DRAW;
        }
        turn = turn == Cell.Bl ? Cell.Wh : Cell.Bl;
        return Result.UNKNOWN;

    }

    private boolean eteded (int row, int column, int row2, int column2) {
        int dr = row - row2;
        int dc = column - column2;
        if (cells[row - dr/2][column - dc/2] == opposite(turn)) {
            return true;
        }
        return false;
    }

    private void clean(int row, int column, int row2, int column2) {
        int dr = row - row2;
        int dc = column - column2;
        cells[row - dr/2][column - dc/2] = Cell.Eb;
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
    public boolean isValid(final Move move) {
        return 0 <= move.getFromcolumn() && 0 <= move.getFromrow() &&
                8 > move.getFromcolumn() && 8 > move.getFromrow() &&
                turn == cells[move.getFromrow()][move.getFromcolumn()] &&
                cells[move.getTorow()][move.getTocolumn()] == Cell.Eb &&
                valid(move.getFromrow(), move.getFromcolumn(), move.getTorow(), move.getTocolumn());
    }

    private boolean valid(int row, int column, int row2, int column2) {
        boolean proverka = false;
        int dr = row - row2;
        int dc = column - column2;
        if (turn == Cell.Wh && dr == -1) {
            return true;
        }
        if (turn == Cell.Bl && dr == 1) {
            return true;
        }
        if (cells[row - dr/2][column - dc/2] == opposite(turn)) {
            return false;
        }
        if (Math.abs(dr) != 2 || Math.abs(dc) != 2) {
            return false;
        }
        if (turn == Cell.Wh && dr > 0) {
            return false;
        }
        if (turn == Cell.Bl && dr < 0) {
            return false;
        }
        return true;
    }

    private Cell opposite(Cell cell) {
        if (cell == Cell.Bl) {
            return Cell.Wh;
        } else {
            return Cell.Bl;
        }
    }

    @Override
    public Cell getCell(final int row, final int column) {
        return cells[row][column];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" 01234567");
        for (int r = 0; r < 8; r++) {
            sb.append("\n");
            sb.append(r);
            for (int c = 0; c < 8; c++) {
                sb.append(SYMBOLS.get(cells[r][c]));
            }
        }
        return sb.toString();
    }
}
