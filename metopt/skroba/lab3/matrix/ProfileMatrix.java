package skroba.lab3.matrix;

import skroba.utils.matrix.Matrix;

import java.util.ArrayList;
import java.util.List;

/**
 * Class for profile matrix implementing {@link Matrix} interface.
 */
public class ProfileMatrix implements Matrix {
	private static double[][] converter(final Matrix matrix) {
		int size = matrix.size();
		final double[][] res = new double[size][size];
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				res[i][j] = matrix.getElement(i, j);
			}
		}
		return res;
	}
	private final int size;
	
	private double[] d;
	private double[] al;
	private double[] au;
	
	private int[] ia;
	private int[] ja;
	
	/**
	 * Constructor from double array.
	 * @param matrix - array of double[][].
	 */
	public ProfileMatrix(final double[][] matrix) {
		size = matrix.length;
		
		extractLow(matrix);
		extractHigh(matrix);
		extractDiagonal(matrix);
	}
	
	/**
	 * Constructor from another matrix.
	 * @param matrix
	 */
	public ProfileMatrix(final Matrix matrix) {
		this(converter(matrix));
	}
	
	@Override
	public double getElement(final int row, final int col) {
		if (row == col) {
			return d[row];
		}
		if (row < col) {
			int start = col - ja[col + 1] + ja[col];
			if (row < start) {
				return 0;
			}
			
			return au[ja[col] + row - start];
		}
		
		int start = row - ia[row + 1] + ia[row];
		if (col < start) {
			return 0;
		}
		
		return al[ia[row] + col - start];
	}
	
	@Override
	public void set(int row, int col, double el) {
		if (row == col) {
			d[row] = el;
			return;
		}
		if (row < col) {
			int start = col - ja[col + 1] + ja[col];
			if (row < start) {
				return;
			}
			
			au[ja[col] + row - start] = el;
			return;
		}
		
		int start = row - ia[row + 1] + ia[row];
		if (col < start) {
			return;
		}
		
		al[ia[row] + col - start] = el;
	}
	
	private void extractHigh(final double[][] matrix) {
		ja = new int[size + 1];
		List<Double> list = new ArrayList<>();
		ja[0] = 0;
		ja[1] = 0;
		for (int i = 0; i < size; i++) {
			int counter = 0;
			for (int j = 0; j < i; j++) {
				if (Math.abs(matrix[j][i] - 0) <= EPS && counter == 0) {
					continue;
				}
				counter++;
				list.add(matrix[j][i]);
			}
			ja[i + 1] = ja[i] + counter;
		}
		au = new double[list.size()];
		for (int i = 0; i < au.length; i++) {
			au[i] = list.get(i);
		}
	}
	
	private void extractLow(final double[][] matrix) {
		ia = new int[size + 1];
		List<Double> list = new ArrayList<>();
		ia[0] = 0;
		ia[1] = 0;
		for (int i = 1; i < size; i++) {
			int counter = 0;
			for (int j = 0; j < i; j++) {
				if (Math.abs(matrix[i][j] - 0) <= EPS && counter == 0) {
					continue;
				}
				counter++;
				list.add(matrix[i][j]);
			}
			ia[i + 1] = ia[i] + counter;
		}
		al = new double[list.size()];
		for (int i = 0; i < al.length; i++) {
			al[i] = list.get(i);
		}
	}
	
	private void extractDiagonal(final double[][] matrix) {
		d = new double[size];
		for (int i = 0; i < size; i++) {
			d[i] = matrix[i][i];
		}
	}
	
	public int firstInL(int i) {
		return i - ia[i + 1] + ia[i];
	}
	
	public int firstInU(int i) {
		return i -  ja[i + 1] + ja[i];
	}
	
	@Override
	public int size() {
		return size;
	}
	
}
