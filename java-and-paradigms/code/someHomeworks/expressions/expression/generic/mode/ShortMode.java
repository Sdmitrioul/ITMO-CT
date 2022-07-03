package expression.generic.mode;

import expression.exception.evaluationException.DivisionByZeroException;
import expression.exception.evaluationException.EvaluationException;

public class ShortMode implements AbstractMode<Short> {
    @Override
    public Short add(Short first, Short second) throws EvaluationException {
        return (short) (first + second);
    }

    @Override
    public Short subtract(Short first, Short second) throws EvaluationException {
        return (short) (first - second);
    }

    @Override
    public Short multiply(Short first, Short second) throws EvaluationException {
        return (short) (first * second);
    }

    @Override
    public Short divide(Short first, Short second) throws EvaluationException {
        if (second == 0) {
            throw new DivisionByZeroException(first + "/" + second);
        }
        return (short) (first / second);
    }

    @Override
    public Short min(Short first, Short second) throws EvaluationException {
        if (first < second) {
            return first;
        } else {
            return second;
        }
    }

    @Override
    public Short max(Short first, Short second) throws EvaluationException {
        if (first > second) {
            return first;
        } else {
            return second;
        }
    }

    @Override
    public Short negate(Short first) throws EvaluationException {
        return (short) - first;
    }

    @Override
    public Short count(Short first) throws EvaluationException {
        return (short) Integer.bitCount(Short.toUnsignedInt(first));
    }

    @Override
    public Short parseNumber(String string) {
        return (short) Integer.parseInt(string);
    }
}
