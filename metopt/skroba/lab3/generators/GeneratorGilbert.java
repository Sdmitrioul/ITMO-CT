package skroba.lab3.generators;

import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

/**
 * Generator of gilbert matrix for third task.
 */
public class GeneratorGilbert extends AbstractGenerator {
	@Override
	public Matrix generate(final int n) {
		double[][] matrix = new double[n][n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				double z = i + j - 1;
				matrix[i][j] = 1 / z;
			}
		}
		return new SquareMatrix(matrix);
	}
}
