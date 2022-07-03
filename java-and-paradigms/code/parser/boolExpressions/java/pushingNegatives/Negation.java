package pushingNegatives;

public class Negation implements Expression {
    private Expression expression;

    public Negation(Expression expression) {
        this.expression = expression;
    }

    @Override
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j,
                            boolean k, boolean l, boolean m, boolean n, boolean o, boolean p, boolean q, boolean r, boolean s, boolean t,
                            boolean u, boolean v, boolean w, boolean x, boolean y, boolean z) {
        return !expression.evaluate(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z);
    }

    @Override
    public Expression doNegation() {
        return expression;
    }

    @Override
    public String toString() {
        return "~" + expression;
    }
}
