package generics.expression.generic.mode;

import generics.expression.exception.evaluationException.EvaluationException;

public interface AbstractMode<T> {
    T add(T first, T second) throws EvaluationException;
    T subtract(T first, T second) throws EvaluationException;
    T multiply(T first, T second) throws EvaluationException;
    T divide(T first, T second) throws EvaluationException;
    T min(T first, T second) throws EvaluationException;
    T max(T first, T second) throws EvaluationException;
    T negate(T first) throws EvaluationException;
    T count(T first) throws EvaluationException;
    T parseNumber(String string);
}
