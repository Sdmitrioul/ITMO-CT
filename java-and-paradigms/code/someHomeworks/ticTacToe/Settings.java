package ticTacToe;

public final class Settings {
    private final int row;
    private final int column;
    private final int lineLength;

    public Settings(final int row, final int column, final int lineLength) {
        this.row = row;
        this.column = column;
        this.lineLength = lineLength;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    public int getLineLength() {
        return lineLength;
    }

    @Override
    public String toString() {
        return "Settings{" +
                "row=" + row +
                ", column=" + column +
                ", lineLength=" + lineLength +
                '}';
    }
}
