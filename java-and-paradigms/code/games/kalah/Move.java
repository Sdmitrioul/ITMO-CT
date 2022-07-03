package kalah;

public final class Move {
    private final int row;
    private final int value;

    public Move(int row, int value) {
        this.row = row;
        this.value = value;
    }

    public int getRow() {
        return row;
    }

    public int getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "row=" + row + ", value=" + value;
    }
}
