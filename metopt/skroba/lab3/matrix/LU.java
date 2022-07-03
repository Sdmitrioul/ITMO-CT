package skroba.lab3.matrix;

import skroba.utils.matrix.Matrix;

/**
 * Class for making LU decomposition from matrix A.
 */
public class LU {
	private final ProfileMatrix matrix;
	public final L L;
	public final U U;
	
	/**
	 * Constructor from {@link ProfileMatrix}.
	 * @param matrix - class contains fields L and U, both are implementing {@link Matrix} interface.
	 */
	public LU(ProfileMatrix matrix) {
		this.matrix = matrix;
		preprocess();
		this.L = new L();
		this.U = new U();
	}
	
	private void preprocess() {
		for (int i = 1; i < matrix.size(); i++) {
			for (int j = matrix.firstInL(i); j < i; j++) {
				double z = matrix.getElement(i, j);
				for (int k = 0; k < j; k++) {
					z -= matrix.getElement(i, k) * matrix.getElement(k, j);
				}
				
				if (z != 0) {
					z /= matrix.getElement(j, j);
				}
				
				matrix.set(i, j, z);
			}
			for (int j = matrix.firstInU(i); j < i; j++) {
				double z = matrix.getElement(j, i);
				for (int k = 0; k < j; k++) {
					z -= matrix.getElement(j, k) * matrix.getElement(k, i);
				}
				matrix.set(j, i, z);
			}
			double z = matrix.getElement(i, i);
			for (int k = 0; k < i; k++) {
				z -= matrix.getElement(i, k) * matrix.getElement(k, i);
			}
			matrix.set(i, i, z);
		}
	}
	
	public class L implements Matrix {
		@Override
		public double getElement(int row, int col) {
			if (row < col) {
				return 0;
			}
			if (row == col) {
				return 1;
			}
			return matrix.getElement(row, col);
		}
		
		@Override
		public void set(int row, int col, double el) {
			throw new IllegalArgumentException("This is unused operation");
		}
		
		@Override
		public int size() {
			return matrix.size();
		}
	}
	public class U implements Matrix {
		@Override
		public double getElement(int row, int col) {
			if (row > col) {
				return 0;
			}
			return matrix.getElement(row, col);
		}
		
		@Override
		public void set(int row, int col, double el) {
			throw new IllegalArgumentException("This is unused operation");
		}
		
		@Override
		public int size() {
			return matrix.size();
		}
	}
}
