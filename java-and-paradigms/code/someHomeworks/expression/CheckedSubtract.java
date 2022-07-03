package expression;

import expression.exceptions.ExpressionCalculatingException;

public class CheckedSubtract extends AbstractOperations {
    public CheckedSubtract(CommonExpression leftOperand, CommonExpression rightOperand) {
        super(leftOperand, rightOperand, "-");
    }

    @Override
    protected int evaluate(int first, int second) throws ExpressionCalculatingException {
        if (second < 0) {
            if (Integer.MAX_VALUE + second < first) {
                throw new ExpressionCalculatingException("Overflow");
            }
        } else {
            if (Integer.MIN_VALUE + second > first) {
                throw new ExpressionCalculatingException("Overflow");
            }
        }
        return first - second;
    }
}
