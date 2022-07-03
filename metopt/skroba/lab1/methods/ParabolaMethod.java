package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;
import skroba.lab1.utils.Parabolic;

import java.util.function.Function;

public class ParabolaMethod extends AbstractMethod {
    public ParabolaMethod(double EPS, double DELTA, Function<Double, Double> function) {
        super("Parabola_Method", EPS, DELTA, "(left-border) (right-border) (x) (x-fun) (middle) (middle-fun)", function);
    }

    @Override
    public Answer findMin(double leftBorder, double rightBorder) throws TimeOutException {
        clear();
        double middlePoint = Parabolic.findMiddlePoint(EPS, function, leftBorder, rightBorder);
        double leftFun = function.apply(leftBorder);
        double rightFun = function.apply(rightBorder);
        double middleFun = function.apply(middlePoint);

        addData(operationCounter, wrapData(leftBorder, rightBorder, middlePoint, middlePoint));

        while ((rightBorder - leftBorder) / 2 > EPS) {
            double x = middlePoint - (Math.pow(middlePoint - leftBorder, 2) * (middleFun - rightFun) - Math.pow(middlePoint - rightBorder, 2) * (middleFun - leftFun)) /
                    (2 * ((middlePoint - leftBorder) * (middleFun - rightFun) - (middlePoint - rightBorder)  * (middleFun - leftFun)));

            double xFun = function.apply(x);

            addData(++operationCounter, wrapData(leftBorder, rightBorder, middlePoint, x));

            if (xFun > middleFun) {
                if (x > middlePoint) {
                    rightBorder = x;
                    rightFun = xFun;
                } else {
                    leftBorder = x;
                    leftFun = xFun;
                }
            } else {
                if (middlePoint >= x) {
                    rightBorder = middlePoint;
                    rightFun = middleFun;
                } else {
                    leftBorder = middlePoint;
                    leftFun = middleFun;
                }
                middlePoint = x;
                middleFun = xFun;
            }

            checkCondition();
        }

        addData(++operationCounter, wrapData(leftBorder, rightBorder, middlePoint, middlePoint));

        return wrapper(middlePoint);
    }
}
