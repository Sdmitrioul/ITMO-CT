package skroba.lab3.generators;

import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

/**
 * Generator of simple matrix with values of elements from 0 ip to 10.
 */
public class GeneratorSimple extends AbstractGenerator{
	@Override
	public Matrix generate(int n) {
		double[][] matrix = new double[n][n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				matrix[i][j] = Math.random() * 3 - 1;
			}
		}
		return new SquareMatrix(matrix);
	}
}
