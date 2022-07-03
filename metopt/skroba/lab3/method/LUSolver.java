package skroba.lab3.method;

import skroba.lab3.matrix.LU;
import skroba.lab3.matrix.ProfileMatrix;
import skroba.utils.matrix.Matrix;
import skroba.utils.Vector;

/**
 * Class that solves SLAU using LU decomposition.
 */
public class LUSolver extends AbstractSolver<ProfileMatrix> {
	private Vector solveFirst(final ProfileMatrix input, final Matrix matrix, final Vector b) {
		double[] y = new double[input.size()];
		
		for (int i = 0; i < input.size(); i++) {
			y[i] = b.get(i);
			for (int j = input.firstInL(i); j < i; j++) {
				y[i] -= matrix.getElement(i, j) * y[j];
				iterations++;
			}
		}
		
		return new Vector(y);
	}
	
	private Vector solveSecond(final ProfileMatrix input, final Matrix matrix, final Vector b)  {
		double[] ans = new double[input.size()];
		
		for (int i = input.size() - 1; i > -1; i--) {
			ans[i] = b.get(i);
			for (int j = input.size() - 1; j > i; j--) {
				ans[i] -= matrix.getElement(i, j) * ans[j];
				iterations++;
			}
			ans[i] /= matrix.getElement(i, i);
			iterations++;
		}
		
		return new Vector(ans);
	}
	
	@Override
	public Vector solve(final ProfileMatrix matrix, final Vector b) {
		iterations = 0;
		LU luMatrices = new LU(matrix);
		
		for (int i = 0; i < matrix.size(); i++) {
			if (Math.abs(luMatrices.U.getElement(i, i)) < EPS) {
				return null;
			}
		}
		
		final Vector answer = solveSecond(matrix, luMatrices.U, solveFirst(matrix, luMatrices.L, b));
		
		return answer;
	}
}
