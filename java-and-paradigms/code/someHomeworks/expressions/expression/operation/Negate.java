package expression.operation;

import expression.TripleExpression;
import expression.exception.evaluationException.EvaluationException;
import expression.generic.mode.AbstractMode;

public class Negate<T> extends AbstractUnaryOperation<T> {
    public Negate(TripleExpression<T> operand, AbstractMode<T> mode) {
        super(operand, mode);
    }

    @Override
    protected T evaluate(T first) throws EvaluationException {
        return mode.negate(first);
    }

    @Override
    public String toString() {
        return "-(" + operand + ")";
    }
}
