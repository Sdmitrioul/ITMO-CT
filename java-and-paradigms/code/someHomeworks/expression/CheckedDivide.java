package expression;

import expression.exceptions.ExpressionCalculatingException;

public class CheckedDivide extends AbstractOperations {
    public CheckedDivide(CommonExpression leftOperand, CommonExpression rightOperand) {
        super(leftOperand, rightOperand, "/");
    }

    @Override
    protected int evaluate(int first, int second) throws ExpressionCalculatingException {
        if (second == 0) {
            throw new ExpressionCalculatingException("Division by zero");
        }
        if (first == Integer.MIN_VALUE && second == -1) {
            throw new ExpressionCalculatingException("Overflow");
        }
        return first / second;
    }
}
