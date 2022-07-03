package skroba.lab1.utils;

import java.util.function.Function;

public interface Parabolic {
    static double findMiddlePoint(double EPS, Function<Double, Double> function, double left, double right) {
        double middle = (right + left) / 2;
        double leftFun = function.apply(left);
        double rightFun = function.apply(right);
        double middleFun = function.apply(middle);

        while ((leftFun < middleFun || rightFun < middleFun) && (right - left) > EPS) {
            if (leftFun < middleFun) {
                right = middle;
                rightFun = function.apply(right);
            } else {
                left = middle;
                leftFun = function.apply(left);
            }

            middle = (right + left) / 2;
            middleFun = function.apply(middle);
        }

        return middle;
    }
}
