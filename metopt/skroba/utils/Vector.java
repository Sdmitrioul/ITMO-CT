package skroba.utils;

import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Simple realization of vector with some methods, based on List.
 */
public class Vector implements Vec<Double> {
	private final List<Double> vector;
	
	/**
	 * Constructor of a new Vector of given List of Double elements.
	 * @param vector - List of Double elements, not null.
	 */
	public Vector(final List<Double> vector) {
		this.vector = vector;
	}
	
	/**
	 * Constructor of a new Vector of given array of double elements.
	 * @param vector -array of double elements, not null.
	 */
	public Vector(final double[] vector) {
		this(Arrays.stream(vector).boxed().collect(Collectors.toList()));
	}
	
	/**
	 * Constructor of new empty vector.
	 */
	public Vector() {
		this(Collections.EMPTY_LIST);
	}
	
	/**
	 * Returns new Vector, product of scalar multiply with this vector.
	 *
	 * @param scalar - scalar value for multiplying
	 * @return - new Vector.
	 */
	public Vector scalarMul(double scalar) {
		return new Vector(vector.stream()
				.map(x -> x * scalar)
				.collect(Collectors.toList())
		);
	}
	
	/**
	 * Multiply this vector with given. Coordinate system is orthogonal.
	 * @param aVector - given vector
	 * @return - result of multiplying.
	 */
	public double mul(Vector aVector) {
		if (size() != aVector.size()) {
			throw new IllegalArgumentException("Vector's must have equals spans");
		}
		
		double ans = 0.0;
		
		for (int i = 0; i < size(); i++) {
			ans += vector.get(i) * aVector.get(i);
		}
		
		return ans;
	}
	
	/**
	 * Sum of this vector and given. This vector and given must have equals lengths.
	 *
	 * @param anotherVector - Vector that is summing to this.
	 * @return - new Vector, equaling sum.
	 */
	public Vector sum(Vector anotherVector) {
		if (size() != anotherVector.size()) {
			throw new IllegalArgumentException("Vectors must have equal lengths");
		}
		
		List<Double> list = new ArrayList<>(size());
		
		for (int i = 0; i < size(); i++) {
			list.add(this.vector.get(i) + anotherVector.get(i));
		}
		
		return new Vector(list);
	}
	
	/**
	 * Returns i coordinate of vector.
	 *
	 * @param i - coordinate number.
	 * @return i coordinate of vector.
	 */
	public Double get(int i) {
		return vector.get(i);
	}
	
	@Override
	public void set(int pos, Double el) {
		vector.set(pos, el);
	}
	
	/**
	 * Returns norma of vector.
	 * @return - norma value of this vector.
	 */
	public double norma() {
		return Math.sqrt(
				this.vector
						.stream()
						.reduce(0.0,
								(a, b) -> a + b * b)
		);
	}
	
	/**
	 * Returns size of vector.
	 * @return - vector size;
	 */
	public int size() {
		return this.vector.size();
	}
	
	/**
	 * Returns copy of this vector.
	 * @return - copy.
	 */
	public Vector copy() {
		return new Vector(new ArrayList<>(this.vector));
	}
	
	/**
	 * Return matrix of multiplication.
	 * @param anotherVector - vector
	 * @return Square matrix
	 */
	public Matrix mulToMatrix(final Vector anotherVector) {
		if (size() != anotherVector.size()) {
			throw new IllegalArgumentException("Vectors must have equal lengths");
		}
		
		double[][] matrix = new double[size()][size()];
		
		for (int i = 0; i < size(); i++) {
			for (int j = 0; j < size(); j++) {
				matrix[i][j] = get(i) * anotherVector.get(j);
			}
		}
		
		return new SquareMatrix(matrix);
	}
	
	@Override
	public String toString() {
		return /*"[" + */this.vector.stream()
				.map(x -> String.format("%.7f", x))
				.collect(Collectors.joining(" "))
				/*+ "]"*/;
	}
}
