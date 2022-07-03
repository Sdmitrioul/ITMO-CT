package skroba.lab4.methods.marquardt.helper;

import skroba.lab3.method.Solver;
import skroba.utils.Vec;
import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

public class HoleckySolver implements Solver<SquareMatrix> {
	private final double EPS = 0.00001;
	/**
	 * Разложение Холецкого.
	 * @param A - input matrix.
	 * @return - разложение.
	 */
	public SquareMatrix decompose(final SquareMatrix A) {
		double[][] L = new double[A.size()][A.size()];
		
		for (int i = 0; i < A.size(); i++) {
			L[i][i] = A.getElement(i, i);
			for (int j = 0; j < i; j++) {
				L[i][i] -= L[i][j] * L[i][j];
			}
			
			L[i][i] = Math.sqrt(L[i][i]);
			
			for (int j = i + 1; j < A.size(); j++) {
				L[j][i] = A.getElement(j, i);
				for (int k = 0; k < i; k++) {
					L[j][i] -= L[j][k] * L[i][k];
				}
				L[j][i] /= L[i][i];
			}
		}
		
		return new SquareMatrix(L);
	}
	
	private void forwardCalculus(final SquareMatrix L, final Vector y) {
		for (int i = 0; i < L.size(); i++) {
			double res = y.get(i);
			
			for (int j = 0; j < i; j++) {
				res -= y.get(j) * L.getElement(i, j);
			}
			
			res /= L.getElement(i, i);
			y.set(i, res);
		}
	}
	
	private void backwardCalculus(final SquareMatrix L, final Vector y) {
		for (int i = L.size() - 1; i >= 0 ; i--) {
			double res = y.get(i);
			
			for (int j = L.size() - 1; j > i; j--) {
				res -= y.get(j) * L.getElement(i, j);
			}
			
			res /= L.getElement(i, i);
			y.set(i, res);
		}
	}
	
	@Override
	public Vector solve(SquareMatrix matrix, Vector b) {
		final SquareMatrix L = decompose(matrix);
		final SquareMatrix transposeL = L.transpose();
		
		Matrix check = L.mul(transposeL);
		
		for (int i = 0; i < matrix.size(); i++) {
			for (int j = 0; j < matrix.size(); j++) {
				if (Double.isNaN(check.getElement(i, j)) || Math.abs(matrix.getElement(i, j) - check.getElement(i, j)) > EPS) {
					return null;
				}
			}
		}
		
		Vector ans = b.copy();
		
		forwardCalculus(L, ans);
		backwardCalculus(transposeL, ans);
		
		return ans;
	}
	
	@Override
	public long getIterations() {
		return 0;
	}
}
