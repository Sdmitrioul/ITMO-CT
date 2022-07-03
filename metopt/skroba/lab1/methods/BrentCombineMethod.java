package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;
import skroba.lab1.utils.Parabolic;

import java.util.function.BinaryOperator;
import java.util.function.Function;

public class BrentCombineMethod extends AbstractMethod {
    private final BinaryOperator<Double> goldenFunction = (left, right) -> left + (3 - Math.sqrt(5)) * (right - left) / 2;

    public BrentCombineMethod(double EPS, double DELTA, Function<Double, Double> function) {
        super("Brant_Combine_Method",EPS, DELTA, "(left-border) (right-border) (x) (x-fun) (w) (w-fun) (v) (v-fun) (u) (u-fun)", function);
    }

    @Override
    public Answer findMin(double leftBorder, double rightBorder) throws TimeOutException {
        clear();
        double x, w, v;
        double xFun, wFun, vFun;
        x = w = v = (leftBorder + rightBorder) / 2;
        xFun = wFun = vFun = function.apply(x);
        double dist, prevDist;
        dist = prevDist = rightBorder - leftBorder;

        addData(operationCounter, wrapData(leftBorder, rightBorder, x, w, v, 0));

        while (true) {
            double g = prevDist;
            double u = 0;
            boolean except = false;
            prevDist = dist;
            double tol = EPS * Math.abs(x) + EPS / 10;
            if (Math.abs(x - (rightBorder + leftBorder) / 2) + (rightBorder - leftBorder) / 2 <= 2 * tol) {
                break;
            }

            if (difference(x, w, v) && difference(xFun, wFun, vFun)) {
                u  = Parabolic.findMiddlePoint(EPS, function, leftBorder, rightBorder);
                if (u < rightBorder && u > leftBorder && Math.abs(u - x) < g / 2) {
                    except = true;
                    if (u - leftBorder < 2 * tol || rightBorder - u < 2 * tol) {
                        u = x - Math.signum(x - (leftBorder + rightBorder) / 2) * tol;
                    }
                }
            }

            if (!except) {
                if (x < (leftBorder + rightBorder) / 2) {
                    u = goldenFunction.apply(x, rightBorder);
                    prevDist = rightBorder - x;
                } else {
                    u = goldenFunction.apply(leftBorder, x);
                    prevDist = x - leftBorder;
                }
            }

            if (Math.abs(u - x) < tol) {
                u = x + Math.signum(u - x) * tol;
            }

            dist = Math.abs(u - x);
            double uFun = function.apply(u);

            if (uFun <= xFun) {
                if (u >= x) {
                    leftBorder = x;
                } else {
                    rightBorder = x;
                }
                v = w;
                w = x;
                x = u;
                vFun = wFun;
                wFun = xFun;
                xFun = uFun;
            } else {
                if (u >= x) {
                    rightBorder = u;
                } else {
                    leftBorder = u;
                }
                if (uFun <= wFun || Math.abs(w - x) < EPS) {
                    v = w;
                    w = u;
                    vFun = wFun;
                    wFun = uFun;
                } else if (uFun <= vFun || Math.abs(v - x) < EPS || Math.abs(v - w) < EPS) {
                    v = u;
                    vFun = uFun;
                }
            }

            addData(++operationCounter, wrapData(leftBorder, rightBorder, x, w, v, u));
            checkCondition();
        }

        double min = (rightBorder + leftBorder) / 2;

        return wrapper(min);
    }

    private boolean difference(double x, double w, double v) {
        return x != w && w != v && x != v;
    }
}
