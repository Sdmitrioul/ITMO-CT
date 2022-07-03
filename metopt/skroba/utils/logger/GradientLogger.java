package skroba.utils.logger;

import skroba.utils.Pair;
import skroba.utils.Vector;

import java.util.List;

public interface GradientLogger extends Logger{
	void log(Pair<Vector, Double> pair);
	List<Pair<Vector, Double>> getData();
}
