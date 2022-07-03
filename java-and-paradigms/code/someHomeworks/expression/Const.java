package expression;

public class Const implements CommonExpression {
    private int value;

    public Const(int value) {
        this.value = value;
    }

    public int evaluate(int x, int y, int z) {
        return value;
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }
}
