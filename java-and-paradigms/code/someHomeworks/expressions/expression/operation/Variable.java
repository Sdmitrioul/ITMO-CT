package expression.operation;

import expression.TripleExpression;
import expression.exception.evaluationException.EvaluationException;
import expression.exception.evaluationException.WrongVariableException;

public class Variable<T> implements TripleExpression<T> {
    private final String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public T evaluate(T x, T y, T z) throws EvaluationException {
        switch (name) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                throw new WrongVariableException(name);
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
