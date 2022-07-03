package dnf;

public class Disjunction extends AbstractOperation {
    public Disjunction(Expression leftOperand, Expression rightOperand) {
        super(leftOperand, rightOperand, "|");
    }

    @Override
    protected boolean evaluate(boolean left, boolean right) {
        return left || right;
    }
}
