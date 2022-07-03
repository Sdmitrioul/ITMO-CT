package dnf;

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
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j) {
        boolean left = leftOperand.evaluate(a, b, c, d, e, f, g, h, i, j);
        boolean right = rightOperand.evaluate(a, b, c, d, e, f, g, h, i, j);
        return evaluate(left, right);
    }

    protected abstract boolean evaluate(boolean left, boolean right);

    @Override
    public String toString() {
        return "(" + leftOperand + " " + deed + " " + rightOperand + ")";
    }
}
