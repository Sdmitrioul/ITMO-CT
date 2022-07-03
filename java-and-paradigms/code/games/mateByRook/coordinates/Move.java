package mateByRook.coordinates;

public final class Move {
    private int fromRow;
    private int fromColumn;
    private int toRow;
    private int toColumn;

    public Move(int fromRow, int fromColumn, int toRow, int toColumn) {
        this.fromRow = fromRow;
        this.fromColumn = fromColumn;
        this.toRow = toRow;
        this.toColumn = toColumn;
    }

    public int getFromRow() {
        return fromRow;
    }

    public int getFromColumn() {
        return fromColumn;
    }

    public int getToRow() {
        return toRow;
    }

    public int getToColumn() {
        return toColumn;
    }

    @Override
    public String toString() {
        return "fromRow=" + fromRow +
                ", fromColumn=" + fromColumn +
                ", toRow=" + toRow +
                ", toColumn=" + toColumn;
    }
}
