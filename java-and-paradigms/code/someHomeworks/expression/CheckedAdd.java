package expression;

import expression.exceptions.ExpressionCalculatingException;

public class CheckedAdd extends AbstractOperations {
    public CheckedAdd(CommonExpression leftOperand, CommonExpression rightOperand) {
        super(leftOperand, rightOperand, "+");
    }

    @Override
    protected int evaluate(int first, int second) throws ExpressionCalculatingException {
        if (second > 0) {
            if (Integer.MAX_VALUE - second < first) {
                throw new ExpressionCalculatingException("overflow");
            }
        } else {
            if (Integer.MIN_VALUE - second > first) {
                throw new ExpressionCalculatingException("overflow");
            }
        }
        return first + second;
    }
}
