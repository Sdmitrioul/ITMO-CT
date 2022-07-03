package fourInLine;

public final class Move {
    private final int column;
    private final Cell value;

    public Move(int column, Cell value) {
        this.column = column;
        this.value = value;
    }

    public int getColumn() {
        return column;
    }

    public Cell getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "column=" + column + ", value=" + value;
    }
}
