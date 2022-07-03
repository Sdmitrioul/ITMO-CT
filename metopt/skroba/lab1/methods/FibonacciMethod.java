package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class FibonacciMethod extends AbstractMethod {
    public ArrayList<Integer> fib = new ArrayList<>(List.of(1, 1));

    private double leftFunction(double left, double right, int index) {
        return left + (right - left) * fib.get(index - 3) / fib.get(index - 1);
    }
    private double rightFunction(double left, double right, int index) {
        return left + (right - left) * fib.get(index - 2) / fib.get(index - 1);
    }

    public FibonacciMethod(double EPS, double DELTA, Function<Double, Double> function) {
        super("Fibonacci_Method",EPS, DELTA, "(left-border) (right-border) (left) (left-fun) (right) (right-fun)", function);
    }

    @Override
    public Answer findMin(double leftBorder, double rightBorder) throws TimeOutException {
        preprocess(leftBorder, rightBorder);
        int index = fib.size();
        double left = leftFunction(leftBorder, rightBorder, index);
        double right = rightFunction(leftBorder, rightBorder, index);
        addData(operationCounter, wrapData(leftBorder, rightBorder, left, right));

        while (index  - 2 != 1) {
            if (function.apply(left) > function.apply(right)) {
                leftBorder = left;
                left = right;
                right = rightFunction(leftBorder, rightBorder, --index);
            } else {
                rightBorder = right;
                right = left;
                left = leftFunction(leftBorder, rightBorder, --index);
            }

            addData(++operationCounter, wrapData(leftBorder, rightBorder, left, right));
            checkCondition();
        }

        right = left + EPS;
        if (function.apply(left) < function.apply(right)) {
            leftBorder = left;
        } else {
            rightBorder = right;
        }

        double min = (leftBorder + rightBorder) / 2;

        return wrapper(min);
    }

    private void preprocess(double leftBorder, double rightBorder) {
        double min = (rightBorder - leftBorder) / EPS;
        while (fib.get(fib.size() - 1) < min) {
            fib.add(fib.get(fib.size() - 1) + fib.get(fib.size() - 2));
        }
    }
}
