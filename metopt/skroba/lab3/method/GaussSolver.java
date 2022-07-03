package skroba.lab3.method;

import skroba.utils.matrix.SquareMatrix;
import skroba.utils.Vector;

/**
 * Class that solves SLAU using gauss method with find lead element.
 */
public class GaussSolver extends AbstractSolver<SquareMatrix> {
	@Override
	public Vector solve(final SquareMatrix matrix, final Vector b) {
		iterations = 0;
		
		if (matrix.size() != b.size()) {
			throw new IllegalArgumentException("Matrix and vector must have equals spans");
		}
		
		if (!makeTriangle(matrix, b)) {
			return null;
		}
		
		return backCalculations(matrix, b);
	}
	
	private Vector backCalculations(final SquareMatrix matrix, final Vector b) {
		double[] ans = new double[matrix.size()];
		
		for (int i = matrix.size() - 1; i >= 0 ; i--) {
			ans[i] = b.get(i);
			for (int j = i + 1; j < matrix.size(); j++) {
				ans[i] -= ans[j] * matrix.getElement(i, j);
				iterations++;
			}
			ans[i] /= matrix.getElement(i, i);
			iterations++;
		}
		
		return new Vector(ans);
	}
	
	private boolean makeTriangle(final SquareMatrix matrix, final Vector b) {
		for (int i = 0; i < matrix.size(); i++) {
			makeLead(matrix, i);
			if (Math.abs(matrix.getElement(i, i)) < EPS) {
				return false;
			}
			
			for (int j = i + 1; j < matrix.size(); j++) {
				double k = matrix.getElement(j, i) / matrix.getElement(i,  i);
				iterations++;
				for (int l = i; l < matrix.size(); l++) {
					matrix.set(j, l, matrix.getElement(j, l) - matrix.getElement(i, l) * k);
					iterations++;
				}
				b.set(j, b.get(j) - b.get(i) * k);
				iterations++;
			}
		}
		
		return true;
	}
	
	private void makeLead(final SquareMatrix matrix, final int row) {
		double max = matrix.getElement(row, row);
		int pos = row;
		for (int i = row + 1; i < matrix.size(); i++) {
			double element = matrix.getElement(i, row);
			if (Math.abs(max) < Math.abs(element)) {
				max = element;
				pos = i;
			}
		}
		
		if (pos == row) {
			return;
		}
		
		for (int i = row; i < matrix.size(); i++) {
			double tmp = matrix.getElement(row, i);
			matrix.set(row, i, matrix.getElement(pos, i));
			matrix.set(pos, i, tmp);
		}
	}
}
