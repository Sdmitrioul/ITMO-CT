package skroba.lab3.generators;

import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

/**
 * Generator of matrix with diagonal predominance. For second task.
 */
public class GeneratorDiagonal extends AbstractGenerator {
	@Override
	public Matrix generate(final int n) {
		return generate(n, 1);
	}
	
	public Matrix generate(final int n, final int k) {
		double[][] matrix = new double[n][n];
		
		for (int i = 0; i < n; i++) {
			int sum = 0;
			for (int j = 0; j < n; j++) {
				if (i != j) {
					int el = (int) (Math.random() * 5) * -1;
					sum += el;
					matrix[i][j] = el;
				}
			}
			matrix[i][i] = -sum;
			if (i == 0) {
				matrix[i][i] += Math.pow(0.1, k);
			}
		}
		return new SquareMatrix(matrix);
	}
	
	public void generate(String prefix, int from, int to, int step, int fromK, int toK) {
		for (int i = from; i <= to; i *= step) {
			String fileDir = prefix + "/" + i +"/";
			for (int j = fromK; j < toK; j++) {
				writeInFile(generate(i, j), fileDir + j);
			}
		}
	}
}
