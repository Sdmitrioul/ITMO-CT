package expression;


import expression.exception.evaluationException.EvaluationException;

public interface TripleExpression<T> extends ToMiniString {
    T evaluate(T x, T y, T z) throws EvaluationException;
}

