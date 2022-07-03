package skroba.lab3.method;

import skroba.utils.matrix.Matrix;

/**
 * Abstract class with returning iterations count.
 * @param <T> - type of input matrix, must implements {@link Matrix}.
 */
public abstract class AbstractSolver<T extends Matrix> implements Solver<T>{
	/**
	 * EPS value.
	 */
	double EPS = 1e-6;
	
	protected long iterations;
	
	@Override
	public long getIterations() {
		return iterations;
	}
}
