package expression;

import expression.exceptions.ExpressionCalculatingException;

public class CheckedNegate implements CommonExpression {
    public CommonExpression expression;

    public CheckedNegate(CommonExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int x, int y, int z) throws ExpressionCalculatingException {
        int res = expression.evaluate(x, y, z);
        if (res == Integer.MIN_VALUE) {
            throw new ExpressionCalculatingException("Overflow");
        }
        return -res;
    }

    @Override
    public String toString() {
        return "-" + expression.toString();
    }
}
