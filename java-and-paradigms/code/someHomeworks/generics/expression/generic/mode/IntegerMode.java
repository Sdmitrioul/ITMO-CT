package generics.expression.generic.mode;

import generics.expression.exception.evaluationException.DivisionByZeroException;
import generics.expression.exception.evaluationException.EvaluationException;
import generics.expression.exception.evaluationException.OverflowException;

public class IntegerMode implements AbstractMode<Integer> {
    @Override
    public Integer add(Integer first, Integer second) throws EvaluationException {
        if (second > 0 && Integer.MAX_VALUE - second < first) {
            throw new OverflowException("add", "answer is more than Integer.MAX_VALUE(" + Integer.MAX_VALUE + ")");
        } else if (second <= 0 && Integer.MIN_VALUE - second > first){
            throw new OverflowException("add", "answer is less than Integer.MIN_VALUE(" + Integer.MIN_VALUE + ")");
        }
        return first + second;
    }

    @Override
    public Integer subtract(Integer first, Integer second) throws EvaluationException {
        if (second < 0 && Integer.MAX_VALUE + second < first) {
            throw new OverflowException("subtract", "answer is more than Integer.MAX_VALUE(" + Integer.MAX_VALUE + ")");
        } else if (second >= 0 && Integer.MIN_VALUE + second > first){
            throw new OverflowException("subtract", "answer is less than Integer.MIN_VALUE(" + Integer.MIN_VALUE + ")");
        }
        return first - second;
    }

    @Override
    public Integer multiply(Integer first, Integer second) throws EvaluationException {
        if (first == 0 || second == 0) {
            return 0;
        } else if ((first > 0 && second > 0 && Integer.MAX_VALUE / second < first)
                || (second < 0 && first > 0 && Integer.MIN_VALUE / first > second)) {
            throw new OverflowException("multiply", "answer is more than Integer.MAX_VALUE(" + Integer.MAX_VALUE + ")");
        } else if ((first < 0 && second < 0 && Integer.MAX_VALUE / second > first)
                || (first < 0 && second > 0 && Integer.MIN_VALUE / second > first)) {
            throw new OverflowException("multiply", "answer is less than Integer.MIN_VALUE(" + Integer.MIN_VALUE + ")");
        }
        return first * second;
    }

    @Override
    public Integer divide(Integer first, Integer second) throws EvaluationException {
        if (second == 0) {
            throw new DivisionByZeroException(first + "/" + second);
        } else if (first == Integer.MIN_VALUE && second == -1) {
            throw new OverflowException("divide", "answer is more than Integer.MAX_VALUE(" + Integer.MAX_VALUE + ")");
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
        if (first == Integer.MIN_VALUE) {
            throw new OverflowException("negate", "answer is more than Integer.MAX_VALUE(" + Integer.MAX_VALUE + ")");
        }
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
