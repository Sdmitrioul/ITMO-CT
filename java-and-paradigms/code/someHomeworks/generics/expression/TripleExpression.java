package generics.expression;


import generics.expression.exception.evaluationException.EvaluationException;

public interface TripleExpression<T> extends ToMiniString {
    T evaluate(T x, T y, T z) throws EvaluationException;
}

