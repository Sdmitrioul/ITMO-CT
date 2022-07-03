package sticks;

public final class Move {
    private int row;
    private int column;
    private boolean isHorizontal;

    public Move(int row, int column, boolean isHorizontal) {
        this.row = row;
        this.column = column;
        this.isHorizontal = isHorizontal;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    public boolean isHorizontal() {
        return isHorizontal;
    }

    @Override
    public String toString() {
        return "row = " + row + ", column = " + column;
    }
}
