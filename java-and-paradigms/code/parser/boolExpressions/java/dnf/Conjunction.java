package dnf;

public class Conjunction extends AbstractOperation {
    public Conjunction(Expression leftOperand, Expression rightOperand) {
        super(leftOperand, rightOperand, "&");
    }

    @Override
    protected boolean evaluate(boolean left, boolean right) {
        return left && right;
    }
}
