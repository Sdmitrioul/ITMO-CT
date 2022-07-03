package expression.generic.mode;

import expression.exception.evaluationException.DivisionByZeroException;
import expression.exception.evaluationException.EvaluationException;

public class DoubleMode implements AbstractMode<Double> {
    @Override
    public Double add(Double first, Double second) {
        return first + second;
    }

    @Override
    public Double subtract(Double first, Double second) {
        return first - second;
    }

    @Override
    public Double multiply(Double first, Double second) {
        return first * second;
    }

    @Override
    public Double divide(Double first, Double second) throws EvaluationException {
        return first / second;
    }

    @Override
    public Double min(Double first, Double second) {
        return Math.min(first, second);
    }

    @Override
    public Double max(Double first, Double second) {
        return Math.max(first, second);
    }

    @Override
    public Double negate(Double first) {
        return -first;
    }

    @Override
    public Double count(Double first) {
        return (double) Long.bitCount(Double.doubleToLongBits(first));
    }

    @Override
    public Double parseNumber(String string) {
        return Double.parseDouble(string);
    }
}
