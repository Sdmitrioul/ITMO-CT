package dnf;

public class Negation implements Expression {
    private Expression expression;

    public Negation(Expression expression) {
        this.expression = expression;
    }

    @Override
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j) {
        return !expression.evaluate(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public String toString() {
        return "~" + expression;
    }
}
