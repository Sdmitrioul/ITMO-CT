package generics.expression.operation;

import generics.expression.TripleExpression;
import generics.expression.exception.evaluationException.EvaluationException;
import generics.expression.generic.mode.AbstractMode;

public class Min<T> extends AbstractOperation<T> {
    public Min(TripleExpression<T> left, TripleExpression<T> right, AbstractMode<T> mode) {
        super(left, right, mode, "min");
    }

    @Override
    protected T evaluate(T first, T second) throws EvaluationException {
        return mode.min(first, second);
    }
}
