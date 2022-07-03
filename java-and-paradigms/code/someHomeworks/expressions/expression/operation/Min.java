package expression.operation;

import expression.TripleExpression;
import expression.exception.evaluationException.EvaluationException;
import expression.generic.mode.AbstractMode;

import java.sql.PseudoColumnUsage;

public class Min<T> extends AbstractOperation<T> {
    public Min(TripleExpression<T> left, TripleExpression<T> right, AbstractMode<T> mode) {
        super(left, right, mode, "min");
    }

    @Override
    protected T evaluate(T first, T second) throws EvaluationException {
        return mode.min(first, second);
    }
}
