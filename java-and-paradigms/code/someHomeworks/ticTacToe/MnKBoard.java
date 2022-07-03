package ticTacToe;

import java.util.Arrays;
import java.util.Map;

public class MnKBoard implements Board, Position {
    final static Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '*'
    );

    private final Cell[][] cells;
    private final Settings settings;
    private Cell turn;
    private int emptyCells;

    public MnKBoard(Settings settings) {
        this.settings = settings;
        this.cells = new Cell[settings.getRow()][settings.getColumn()];
        emptyCells = settings.getColumn() * settings.getRow();
        for (Cell[] row: cells) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
    }

    private boolean checkWinningMove(int row, int column) {
        int maxLine = checkLine(row, column, 1, 0);
        maxLine = Math.max(checkLine(row, column, 0, 1), maxLine);
        maxLine = Math.max(checkLine(row, column, 1, 1), maxLine);
        maxLine = Math.max(checkLine(row, column, 1, -1), maxLine);
        return maxLine >= settings.getLineLength();
    }

    private int checkLine(int row, int column, int deltaY, int deltaX) {
        int count =  1;
        int y = row;
        int x = column;
        while (y + deltaY < settings.getRow() && y + deltaY >= 0 && x + deltaX >= 0 && x + deltaX < settings.getColumn() && cells[y + deltaY][x + deltaX] == cells[y][x]) {
            count++;
            y += deltaY;
            x += deltaX;
        }
        y = row;
        x = column;
        while (y - deltaY < settings.getRow() && y - deltaY >= 0 && x - deltaX >= 0 && x - deltaX < settings.getColumn() && cells[y - deltaY][x - deltaX] == cells[y][x]) {
            count++;
            y -= deltaY;
            x -= deltaX;
        }
        return  count;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        cells[move.getRow()][move.getColumn()] = move.getValue();
        emptyCells--;
        int i = move.getRow(), j = move.getColumn();
        if (checkWinningMove(move.getRow(), move.getColumn())){
            return Result.WIN;
        }
        if (emptyCells == 0) {
            return Result.DRAW;
        }
        turn = turn == Cell.X ? Cell.O : Cell.X;
        return Result.UNKNOWN;
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
    public Settings getSettings() {
        return settings;
    }

    @Override
    public boolean isValid(Move move) {
        return 0 <= move.getRow() && move.getRow() < settings.getRow()
                && 0 <= move.getColumn() && move.getColumn() < settings.getColumn()
                && cells[move.getRow()][move.getColumn()] == Cell.E
                && turn == getCell();
    }

    @Override
    public Cell getCell(int row, int column) {
        return cells[row][column];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" ");
        for (int i = 0; i < settings.getColumn(); i++) {
            sb.append(i);
        }
        for (int r = 0; r < settings.getRow(); r++) {
            sb.append("\n");
            sb.append(r);
            for (int c = 0; c < settings.getColumn(); c++) {
                sb.append(SYMBOLS.get(cells[r][c]));
            }
        }
        return sb.toString();
    }
}
