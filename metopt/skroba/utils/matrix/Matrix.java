package skroba.utils.matrix;

import skroba.utils.Vec;
import skroba.utils.Vector;

import java.util.ArrayList;
import java.util.List;

public interface Matrix{
	double EPS = 1e-20;
	double getElement(int row, int col);
	void set(int row, int col, double el);
	default Matrix scalarMul(double alpha) {
		final double[][] resMatrix = new double[size()][size()];
		
		for (int i = 0; i < this.size(); i++) {
			for (int j = 0; j < size(); j++) {
				resMatrix[i][j] = getElement(i, j) * alpha;
			}
		}
		
		return new SquareMatrix(resMatrix);
	}
	default Vector mul(Vector vec) {
		if (size() != vec.size()) {
			throw new IllegalArgumentException("Vector must have equal span to matrix");
		}
		
		final List<Double> ans = new ArrayList<>(this.size());
		
		for (int i = 0; i < this.size(); i++) {
			double accum = 0.0;
			for (int j = 0; j < size(); j++) {
				accum += getElement(j, i) * vec.get(j);
			}
			ans.add(accum);
		}
		return new Vector(ans);
	}
	default SquareMatrix sum(Matrix matrix) {
		if (size() != matrix.size()) {
			throw new IllegalArgumentException("Given matrix must have equal span to matrix");
		}
		
		final double[][] resMatrix = new double[size()][size()];
		
		for (int i = 0; i < this.size(); i++) {
			for (int j = 0; j < size(); j++) {
				resMatrix[i][j] = getElement(i, j) + matrix.getElement(i, j);
			}
		}
		
		return new SquareMatrix(resMatrix);
	}
	default Matrix sub(Matrix matrix) {
		return this.sum(matrix.scalarMul(-1));
	}
	int size();
}
