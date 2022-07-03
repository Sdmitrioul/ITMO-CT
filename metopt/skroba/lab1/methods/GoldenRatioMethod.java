package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;

import java.util.function.BinaryOperator;
import java.util.function.Function;

public class GoldenRatioMethod extends AbstractMethod {
    private final BinaryOperator<Double> leftFunction = (left, right) -> left + (3 - Math.sqrt(5)) * (right - left) / 2;
    private final BinaryOperator<Double> rightFunction = (left, right) -> left + (Math.sqrt(5) - 1) * (right - left) / 2;

    public GoldenRatioMethod(final double EPS, final double DELTA, final Function<Double, Double> function) {
        super("Golden_Ratio_Method",EPS, DELTA, "(left-border) (right-border) (left) (left-fun) (right) (right-fun)", function);
    }

    @Override
    public Answer findMin(double leftBorder, double rightBorder) throws TimeOutException {
        clear();
        double left = leftFunction.apply(leftBorder, rightBorder);
        double right = rightFunction.apply(leftBorder, rightBorder);

        addData(operationCounter, wrapData(leftBorder, rightBorder, left, right));

        while ((rightBorder - leftBorder) / 2 > EPS) {
            if (function.apply(left) <= function.apply(right)) {
                rightBorder = right;
                right = left;
                left = leftFunction.apply(leftBorder, rightBorder);
            } else {
                leftBorder = left;
                left = right;
                right = rightFunction.apply(leftBorder, rightBorder);
            }

            addData(++operationCounter, wrapData(leftBorder, rightBorder, left, right));
            checkCondition();
        }

        double min = (leftBorder + rightBorder) / 2;

        return wrapper(min);
    }
}
