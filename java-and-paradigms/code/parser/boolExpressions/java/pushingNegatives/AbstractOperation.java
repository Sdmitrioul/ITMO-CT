package pushingNegatives;

public abstract class AbstractOperation implements Expression {
    private Expression leftOperand;
    private Expression rightOperand;
    private String deed;

    public AbstractOperation(Expression leftOperand, Expression rightOperand, String deed) {
        this.leftOperand = leftOperand;
        this.rightOperand = rightOperand;
        this.deed = deed;
    }

    @Override
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j,
                            boolean k, boolean l, boolean m, boolean n, boolean o, boolean p, boolean q, boolean r, boolean s, boolean t,
                            boolean u, boolean v, boolean w, boolean x, boolean y, boolean z) {
        boolean left = leftOperand.evaluate(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z);
        boolean right = rightOperand.evaluate(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z);
        return evaluate(left, right);
    }

    protected abstract boolean evaluate(boolean left, boolean right);

    public Expression doNegation() {
        if (deed.equals("|")) {
            return new Conjunction(leftOperand.doNegation(), rightOperand.doNegation());
        } else {
            return new Disjunction(leftOperand.doNegation(), rightOperand.doNegation());
        }
    }

    /*protected abstract String toSmallString();*/

    @Override
    public String toString() {
        if (this.getClass() == Conjunction.class) {
            if (leftOperand.getClass() == Disjunction.class && rightOperand.getClass() == Disjunction.class) {
                return "(" + leftOperand + ") " + deed + " (" + rightOperand + ")";
            } else if (leftOperand.getClass() == Disjunction.class) {
                return "(" + leftOperand + ") " + deed + " " + rightOperand;
            } else if (rightOperand.getClass() == Disjunction.class) {
                return leftOperand + " " + deed + " (" + rightOperand + ")";
            }
        }
        return leftOperand + " " + deed + " " + rightOperand;
    }
}
