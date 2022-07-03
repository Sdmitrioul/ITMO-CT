package generics.expression.generic.mode;

import generics.expression.exception.evaluationException.DivisionByZeroException;
import generics.expression.exception.evaluationException.EvaluationException;

public class LongMode implements AbstractMode<Long> {
    @Override
    public Long add(Long first, Long second) throws EvaluationException {
        return first + second;
    }

    @Override
    public Long subtract(Long first, Long second) throws EvaluationException {
        return first - second;
    }

    @Override
    public Long multiply(Long first, Long second) throws EvaluationException {
        return first * second;
    }

    @Override
    public Long divide(Long first, Long second) throws EvaluationException {
        if (second == 0) {
            throw new DivisionByZeroException(first + "/" + second);
        }
        return first / second;
    }

    @Override
    public Long min(Long first, Long second) throws EvaluationException {
        return Math.min(first, second);
    }

    @Override
    public Long max(Long first, Long second) throws EvaluationException {
        return Math.max(first, second);
    }

    @Override
    public Long negate(Long first) throws EvaluationException {
        return - first;
    }

    @Override
    public Long count(Long first) throws EvaluationException {
        return (long) Long.bitCount(first);
    }

    @Override
    public Long parseNumber(String string) {
        return Long.parseLong(string);
    }
}
