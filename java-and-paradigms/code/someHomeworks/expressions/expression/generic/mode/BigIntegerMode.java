package expression.generic.mode;

import expression.exception.evaluationException.DivisionByZeroException;
import expression.exception.evaluationException.EvaluationException;

import java.math.BigInteger;

public class BigIntegerMode implements AbstractMode<BigInteger> {
    @Override
    public BigInteger add(BigInteger first, BigInteger second) throws EvaluationException {
        return first.add(second);
    }

    @Override
    public BigInteger subtract(BigInteger first, BigInteger second) throws EvaluationException {
        return first.subtract(second);
    }

    @Override
    public BigInteger multiply(BigInteger first, BigInteger second) throws EvaluationException {
        return first.multiply(second);
    }

    @Override
    public BigInteger divide(BigInteger first, BigInteger second) throws EvaluationException {
        try {
            return first.divide(second);
        } catch (ArithmeticException e) {
            throw new DivisionByZeroException(first + "/" + second);
        }
    }

    @Override
    public BigInteger min(BigInteger first, BigInteger second) throws EvaluationException {
        return first.min(second);
    }

    @Override
    public BigInteger max(BigInteger first, BigInteger second) throws EvaluationException {
        return first.max(second);
    }

    @Override
    public BigInteger negate(BigInteger first) throws EvaluationException {
        return first.negate();
    }

    @Override
    public BigInteger count(BigInteger first) throws EvaluationException {
        return BigInteger.valueOf(first.bitCount());
    }

    @Override
    public BigInteger parseNumber(String string) {
        return new BigInteger(string);
    }
}
