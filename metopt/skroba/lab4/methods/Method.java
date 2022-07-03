package skroba.lab4.methods;

import skroba.lab4.functions.FunctionData;
import skroba.utils.Vector;
import skroba.utils.logger.Logger;

public interface Method {
	Vector findSolution(final Vector value, final FunctionData function);
	long getIterations();
	void setLogger(Logger logger);
}
