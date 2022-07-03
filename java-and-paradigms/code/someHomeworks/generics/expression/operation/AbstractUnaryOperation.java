package generics.expression.operation;

import generics.expression.TripleExpression;
import generics.expression.exception.evaluationException.EvaluationException;
import generics.expression.generic.mode.AbstractMode;

public abstract class AbstractUnaryOperation<T> implements TripleExpression<T> {
    protected final TripleExpression<T> operand;
    protected final AbstractMode<T> mode;

    public AbstractUnaryOperation(TripleExpression<T> operand, AbstractMode<T> mode) {
        this.operand = operand;
        this.mode = mode;
    }

    protected abstract T evaluate(T first) throws EvaluationException;

    @Override
    public T evaluate(T x, T y, T z) throws EvaluationException {
        return evaluate(operand.evaluate(x, y, z));
    }
}
