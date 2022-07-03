package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;

import java.util.function.Function;

public class DichotomyMethod extends AbstractMethod {
    public DichotomyMethod(double EPS, double DELTA, Function<Double, Double> function) {
        super("Dichotomy_Method", EPS, DELTA, "(left-border) (right-border) (left) (left-fun) (right) (right-fun)", function);
    }

    @Override
    public Answer findMin(double leftBorder, double rightBorder) throws TimeOutException {
        clear();
        double left = (leftBorder + rightBorder - DELTA) / 2;
        double right = (leftBorder + rightBorder + DELTA) / 2;

        addData(operationCounter, wrapData(leftBorder, rightBorder, left, right));

        while (Math.abs(rightBorder - leftBorder) > 2.0 * EPS) {
            if (function.apply(left) <= function.apply(right)) {
                rightBorder = right;
            } else {
                leftBorder = left;
            }

            left = (leftBorder + rightBorder - DELTA) / 2.0;
            right = (leftBorder + rightBorder + DELTA) / 2.0;

            addData(++operationCounter, wrapData(leftBorder, rightBorder, left, right));
            checkCondition();
        }

        double min = (rightBorder + leftBorder) / 2;

        return wrapper(min);
    }
}
