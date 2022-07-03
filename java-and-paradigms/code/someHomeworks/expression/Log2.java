package expression;

import expression.exceptions.ExpressionCalculatingException;

public class Log2 implements CommonExpression {
    public CommonExpression expression;

    public Log2(CommonExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int x, int y, int z) throws ExpressionCalculatingException {
        int res = expression.evaluate(x, y, z);
        if (res <= 0)
            throw new ExpressionCalculatingException("Logarithm of negative value");
        int number = 1;
        int ch = 0;
        while (res != 1) {
            ch++;
            res = res / 2;
        }
        return ch;
    }
}
