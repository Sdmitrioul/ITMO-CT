package expression.operation;

import expression.TripleExpression;
import expression.exception.evaluationException.EvaluationException;
import expression.generic.mode.AbstractMode;

public class Max<T> extends AbstractOperation<T> {
    public Max(TripleExpression<T> left, TripleExpression<T> right, AbstractMode<T> mode) {
        super(left, right, mode, "Max");
    }

    @Override
    protected T evaluate(T first, T second) throws EvaluationException {
        return mode.max(first, second);
    }
}
