package expression.operation;

import expression.TripleExpression;
import expression.exception.evaluationException.EvaluationException;
import expression.generic.mode.AbstractMode;

public class Add<T> extends AbstractOperation<T> {
    public Add(TripleExpression<T> left, TripleExpression<T> right, AbstractMode<T> mode) {
        super(left, right, mode, "+");
    }

    @Override
    protected T evaluate(T first, T second) throws EvaluationException {
        return mode.add(first, second);
    }
}
