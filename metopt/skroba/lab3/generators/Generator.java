package skroba.lab3.generators;

import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;

public interface Generator {
	/**
	 * Generate matrix.
	 * @param n - span of dim.
	 * @return - {@link Matrix} generated matrix.
	 */
	Matrix generate(int n);
	
	/**
	 * Generate matrices in files with dimes from to span size.
	 * @param prefix - prefix for files names.
	 * @param from - min span.
	 * @param to - max span.
	 * @param step - step of dims size.
	 */
	void generate(String prefix, int from, int to, int step);
	
	/**
	 * Write matrix in file.
	 * @param matrix - {@link Matrix} matrix to write.
	 * @param fileName - name of file in which write matrix.
	 */
	void writeInFile(Matrix matrix, String fileName);
	
	/**
	 * Generate random vector.
	 * @param n - span.
	 * @return - {@link skroba.utils.Vector} with values from 0 up to 13.
	 */
	static Vector generateVector(int n) {
		double[] vector = new double[n];
		for (int i = 0; i < n; i++) {
			vector[i] = Math.random() * 13;
		}
		return new Vector(vector);
	}
}
