package generics.expression.generic.mode;

import generics.expression.exception.evaluationException.DivisionByZeroException;
import generics.expression.exception.evaluationException.EvaluationException;

public class IntMode implements AbstractMode<Integer> {
    @Override
    public Integer add(Integer first, Integer second) throws EvaluationException {
        return first + second;
    }

    @Override
    public Integer subtract(Integer first, Integer second) throws EvaluationException {
        return first - second;
    }

    @Override
    public Integer multiply(Integer first, Integer second) throws EvaluationException {
        return first * second;
    }

    @Override
    public Integer divide(Integer first, Integer second) throws EvaluationException {
        if (second == 0) {
            throw new DivisionByZeroException(first + "/" + second);
        }
        return first / second;
    }

    @Override
    public Integer min(Integer first, Integer second) {
        if (first < second) {
            return first;
        } else {
            return second;
        }
    }

    @Override
    public Integer max(Integer first, Integer second) {
        if (first > second) {
            return first;
        } else {
            return second;
        }
    }

    @Override
    public Integer negate(Integer first) throws EvaluationException {
        return -first;
    }

    @Override
    public Integer count(Integer first) {
        return Integer.bitCount(first);
    }

    @Override
    public Integer parseNumber(String string) {
        return Integer.parseInt(string);
    }
}
