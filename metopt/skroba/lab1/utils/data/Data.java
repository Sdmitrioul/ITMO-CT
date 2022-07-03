package skroba.lab1.utils.data;

import java.util.List;

public class Data {
    public final double leftBorder;
    public final double rightBorder;
    public final double diff;
    public final List<Pair<Double, Double>> pairs;

    public Data(double leftBorder, double rightBorder, double diff, List<Pair<Double, Double>> pairs) {
        this.leftBorder = leftBorder;
        this.rightBorder = rightBorder;
        this.diff = diff;
        this.pairs = pairs;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(leftBorder).append(" ").append(rightBorder).append(" ").append(diff).append(" ");
        for (Pair<Double, Double> pair : pairs) {
            builder.append(pair.first).append(" ").append(pair.second).append(" ");
        }
        return builder.toString();
    }
}
