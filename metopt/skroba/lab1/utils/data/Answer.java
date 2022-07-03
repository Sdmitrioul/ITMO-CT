package skroba.lab1.utils.data;

import java.util.HashMap;
import java.util.LinkedHashMap;

public final class Answer {
    private final double min;
    private final int operationsCounter;
    private final String methodName;
    private final LinkedHashMap<Integer, Data> data;
    private final String nameOfParameters;

    public Answer(double min, LinkedHashMap<Integer, Data> data, String nameOfParameters, String methodName) {
        this.min = min;
        this.methodName = methodName;
        this.data = data;
        this.nameOfParameters = nameOfParameters;
        this.operationsCounter = data != null ? data.size() : 0;
    }

    public double getMin() {
        return min;
    }
    
    public int getOperationsCounter() {
        return operationsCounter;
    }
    
    public HashMap<Integer, Data> getData() {
        return data;
    }

    public String getNameOfParameters() {
        return nameOfParameters;
    }

    public String getMethodName() {
        return methodName;
    }
}
