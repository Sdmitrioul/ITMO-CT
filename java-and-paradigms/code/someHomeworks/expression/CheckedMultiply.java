package expression;

import expression.exceptions.ExpressionCalculatingException;

public class CheckedMultiply extends AbstractOperations {
    public CheckedMultiply(CommonExpression leftOperand, CommonExpression rightOperand) {
        super(leftOperand, rightOperand, "*");
    }

    @Override
    protected int evaluate(int first, int second) throws ExpressionCalculatingException {
        if (first == 0 || second == 0) {
            return 0;
        }
        if (first > 0 && second > 0) {
            if (Integer.MAX_VALUE / second < first)
                throw new ExpressionCalculatingException("Overflow");
        } else if (first < 0 && second < 0) {
            if (Integer.MAX_VALUE / second > first)
                throw new ExpressionCalculatingException("Overflow");
        } else {
            if (first < 0) {
                if (Integer.MIN_VALUE / second > first)
                    throw new ExpressionCalculatingException("Overflow");
            } else {
                if (second < 0) {
                    if (Integer.MIN_VALUE / first > second)
                        throw new ExpressionCalculatingException("Overflow");
                }
            }
        }
        return first * second;
    }
}
