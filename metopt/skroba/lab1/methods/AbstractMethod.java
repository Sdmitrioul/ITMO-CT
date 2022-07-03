package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;
import skroba.lab1.utils.data.Data;
import skroba.lab1.utils.data.Pair;

import java.util.*;
import java.util.function.Function;

public abstract class AbstractMethod implements MinimumSearcher {
    private final int MAX_OPERATION_NUMBER = 1000000;
    private boolean exception = false;
    private final String methodName;
    protected final double EPS;
    protected final double DELTA;
    protected final String nameOfParameters;
    protected final Function<Double, Double> function;
    protected int operationCounter = 1;
    protected LinkedHashMap<Integer, Data> data = new LinkedHashMap<>();


    public AbstractMethod(String methodName, double EPS, double DELTA,
                          String nameOfParameters, Function<Double, Double> function) {
        this.methodName = methodName;
        this.EPS = EPS;
        this.DELTA = DELTA;
        this.nameOfParameters = nameOfParameters;
        this.function = function;
    }

    @Override
    public abstract Answer findMin(double leftBorder, double rightBorder) throws TimeOutException;

    @Override
    public int getOperationCounter() {
        return operationCounter;
    }

    protected Answer wrapper(final double min) {
        return new Answer(min, data,
                nameOfParameters, this.methodName);
    }

    protected void addData(final int operation, Data data) {
        this.data.put(operation, data);
    }

    protected List<Pair<Double, Double>> pointsWrapper(final double ... points) {
        ArrayList<Pair<Double, Double>> list = new ArrayList<>();
        for (double point : points) {
            list.add(new Pair<>(point, function.apply(point)));
        }
        return list;
    }

    protected void checkCondition() throws TimeOutException {
        if (operationCounter > MAX_OPERATION_NUMBER) {
            exception = true;
            throw new TimeOutException(methodName + " can't find operation in needed time");
        }
    }

    public Answer answerWithException() {
        if (exception) {
            return wrapper(Double.NaN);
        }
        return null;
    }

    protected Data wrapData(final double leftBorder, final double rightBorder, final double ... points) {
        return new Data(leftBorder, rightBorder, rightBorder - leftBorder, pointsWrapper(points));
    }

    protected void clear() {
        operationCounter = 1;
        exception = false;
        data = new LinkedHashMap<>();
    }
}
