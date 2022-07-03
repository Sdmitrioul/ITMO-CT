package expression;

import expression.exceptions.ExpressionCalculatingException;

public abstract class AbstractOperations implements CommonExpression {
    private CommonExpression rightOperand;
    private CommonExpression leftOperand;
    private String operand;

    public AbstractOperations(CommonExpression leftOperand, CommonExpression rightOperand, String operand) {
        this.leftOperand = leftOperand;
        this.rightOperand = rightOperand;
        this.operand = operand;
    }

    @Override
    public int evaluate(int x, int y, int z) throws ExpressionCalculatingException {
        int first = leftOperand.evaluate(x, y, z);
        int second = rightOperand.evaluate(x, y, z);
        return evaluate(first, second);
    }

    protected abstract int evaluate(int first, int second) throws ExpressionCalculatingException;

    @Override
    public String toString() {
        return leftOperand.toString() + operand + rightOperand.toString();
    }
}
