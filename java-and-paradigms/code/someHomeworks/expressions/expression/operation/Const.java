package expression.operation;

import expression.TripleExpression;

public class Const<T> implements TripleExpression<T> {
    private final T value;

    public Const(T value) {
        this.value = value;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return value;
    }

    @Override
    public String toString() {
        return value.toString();
    }
}
