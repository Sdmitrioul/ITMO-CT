package skroba.utils.matrix;

import skroba.utils.Vector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Simple realization of {@link SquareMatrix}.
 */
public class SquareMatrix implements Matrix {
	private final List<Vector> matrix;
	
	/**
	 * Constructor from List of List of Doubles.
	 * @param matrix - List of List of Doubles.
	 */
	public SquareMatrix(final List<List<Double>> matrix) {
		this(matrix.stream()
				.map(Vector::new)
				.toArray(Vector[]::new)
		);
	}
	
	/**
	 * Constructor diagonal matrix with given size with given value.
	 * @param size - size of matrix.
	 * @param value - value of diag.
	 */
	public SquareMatrix(final int size, final double value) {
		this(createDiagMatrix(size, value));
	}
	
	/**
	 * Constructor from array of arrays.
	 * @param matrix
	 */
	public SquareMatrix(final double[][] matrix) {
		this(Arrays.stream(matrix).map(x -> Arrays.stream(x)
				.boxed()
				.collect(Collectors.toList()))
				.collect(Collectors.toList()));
	}
	
	/**
	 * Constructor from array of Vectors.
	 * @param matrix
	 */
	public SquareMatrix(final Vector[] matrix) {
		final int length = matrix.length;
		if (Arrays.stream(matrix).anyMatch(x -> x.size() != length)) {
			throw new IllegalArgumentException("Matrix must be square!");
		}
		this.matrix = Arrays.stream(matrix).collect(Collectors.toList());
	}
	
	/**
	 * Create diag matrix with given size with given value.
	 * @param size - size of matrix.
	 * @param value - diag element values.
	 * @return - double[][] array, represent matrix.
	 */
	public static double[][] createDiagMatrix(final int size, final double value) {
		double[][] matrix = new double[size][size];
		for (int i = 0; i < size; i++) {
			matrix[i][i] = value;
		}
		return matrix;
	}
	
	/**
	 * Multiplying matrix with vector. Coordinate system is orthogonal.
	 * @param vector multiplying vector.
	 * @return vector, result of multiplying.
	 */
	public Vector mul(Vector vector) {
		if (vector.size() != matrix.size()) {
			throw new IllegalArgumentException("Matrix and vector must have equals spans");
		}
		
		final int span = vector.size();
		List<Double> ans = new ArrayList<>(span);
		
		for (int i = 0; i < vector.size(); i++) {
			ans.add(matrix.get(i).mul(vector));
		}
		
		return new Vector(ans);
	}
	
	/**
	 * Returns element of matrix. on position (row, element).
	 * @param row - row of matrix.
	 * @param column - column of matrix.
	 * @return - element on position (row, element).
	 */
	public double getElement(final int row, final int column) {
		return matrix.get(row).get(column);
	}
	
	@Override
	public void set(int row, int col, double el) {
		matrix.get(row).set(col, el);
	}
	
	/**
	 * Returns {@link Vector} from row of matrix.
	 * @param  - row number.
	 * @return - Vector.
	 */
	public Vector getVector(final int i) {
		return matrix.get(i);
	}
	
	/**
	 * Returns size of squarre matrix.
	 *
	 * @return - size.
	 */
	public int size() {
		return matrix.size();
	}
	
	public Matrix copy() {
		double[][] matrix = new double[size()][size()];
		for (int i = 0; i < size(); i++) {
			for (int j = 0; j < size(); j++) {
				matrix[i][j] = getElement(i, j);
			}
		}
		return new SquareMatrix(matrix);
	}
	
	public SquareMatrix transpose() {
		double[][] matrix = new double[size()][size()];
		
		for (int i = 0; i < size(); i++) {
			for (int j = 0; j < size(); j++) {
				matrix[i][j] = this.matrix.get(j).get(i);
			}
		}
		
		return new SquareMatrix(matrix);
	}
	
	public SquareMatrix mul(Matrix B) {
		if (B.size() != size()) {
			throw new IllegalArgumentException("Matrix must be equals!");
		}
		
		double[][] res = new double[size()][size()];
		
		for (int i = 0; i < size(); i++) {
			for (int j = 0; j < size(); j++) {
				for (int k = 0; k < size(); k++) {
					res[i][j] += matrix.get(i).get(k) * B.getElement(k, j);
				}
			}
		}
		
		return new SquareMatrix(res);
	}
	
	@Override
	public String toString() {
		return "["
				+ matrix.stream()
				.map(Vector::toString)
				.collect(Collectors
						.joining(",\n"))
				+ "]";
	}
}
