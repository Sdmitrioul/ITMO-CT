package chekers;

public final class Move {
    private final int fromrow;
    private final int fromcolumn;
    private final int torow;
    private final int tocolumn;
    private final Cell value;

    public Move(int fromrow, int fromcolumn, int torow, int tocolumn, Cell value) {
        this.fromrow = fromrow;
        this.fromcolumn = fromcolumn;
        this.torow = torow;
        this.tocolumn = tocolumn;
        this.value = value;
    }

    public int getFromrow() {
        return fromrow;
    }

    public int getFromcolumn() {
        return fromcolumn;
    }

    public int getTorow() {
        return torow;
    }

    public int getTocolumn() {
        return tocolumn;
    }

    public Cell getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "Move{" +
                "fromrow=" + fromrow +
                ", fromcolumn=" + fromcolumn +
                ", torow=" + torow +
                ", tocolumn=" + tocolumn +
                '}';
    }
}
