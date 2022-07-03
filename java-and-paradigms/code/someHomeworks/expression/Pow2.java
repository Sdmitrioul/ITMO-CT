package expression;

import expression.exceptions.ExpressionCalculatingException;

public class Pow2 implements CommonExpression {
    private CommonExpression commonExpression;

    public Pow2(CommonExpression commonExpression) {
        this.commonExpression = commonExpression;
    }

    @Override
    public int evaluate(int x, int y, int z) throws ExpressionCalculatingException {
        int max = Integer.MAX_VALUE / 2;
        int res = 1;
        int n = commonExpression.evaluate(x, y, z);
        if (n < 0)
            throw new ExpressionCalculatingException("Double value");
        for (int i = 0; i < n; i++) {
            if (res > max) {
                throw new ExpressionCalculatingException("Overflow");
            } else {
                res = res * 2;
            }
        }
        return res;
    }
}
