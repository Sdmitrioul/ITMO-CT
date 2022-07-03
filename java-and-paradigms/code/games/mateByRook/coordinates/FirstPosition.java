package mateByRook.coordinates;

import mateByRook.Cell;

public final class FirstPosition {
    private Cell cell;
    private int row;
    private int column;

    public FirstPosition(Cell cell, int row, int column) {
        this.cell = cell;
        this.row = row;
        this.column = column;
    }

    public Cell getCell() {
        return cell;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    @Override
    public String toString() {
        return "FirstPosition: " +
                "cell = " + cell +
                ", row = " + row +
                ", column = " + column;
    }
}
