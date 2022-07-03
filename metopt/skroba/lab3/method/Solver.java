package skroba.lab3.method;


import skroba.utils.matrix.Matrix;
import skroba.utils.Vector;

/**
 * Interface for SLAU solving;
 * @param <T>
 */
public interface Solver<T extends Matrix> {
	/**
	 * Solve Ax = b task.
	 * @param matrix - A matrix.
	 * @param b - b vector.
	 * @return - x vector.
	 */
	Vector solve(final T matrix, final Vector b);
	
	/**
	 * Returns count of made calculations.
	 * @return - long value, count of iterations.
	 */
	long getIterations();
}
