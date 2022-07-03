package expression.operation;

import expression.TripleExpression;
import expression.exception.evaluationException.EvaluationException;
import expression.generic.mode.AbstractMode;

public abstract class AbstractOperation<T> implements TripleExpression<T> {
    protected final TripleExpression<T> left;
    protected final TripleExpression<T> right;
    protected final AbstractMode<T> mode;
    private final String operand;

    public AbstractOperation(TripleExpression<T> left, TripleExpression<T> right, AbstractMode<T> mode, String operand) {
        this.left = left;
        this.right = right;
        this.mode = mode;
        this.operand = operand;
    }

    protected abstract T evaluate(T first, T second) throws EvaluationException;

    @Override
    public T evaluate(T x, T y, T z) throws EvaluationException {
        T first = left.evaluate(x, y, z);
        T second  = right.evaluate(x, y, z);
        return evaluate(first, second);
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " " + operand + " " + right.toString() + ")";
    }
}
