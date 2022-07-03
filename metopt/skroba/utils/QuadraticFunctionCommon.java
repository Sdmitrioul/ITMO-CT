package skroba.utils;

import skroba.lab4.functions.FunctionData;
import skroba.utils.matrix.SquareMatrix;

import java.util.function.Function;

/**
 *	Simple realization of algebra quadratic function. Using {@link SquareMatrix} and {@link Vector}.
 */
public class QuadraticFunctionCommon implements Function<Vector, Double> {
	private int span;
	public SquareMatrix aMatrix;
	public Vector bVector;
	public double constant;
	public FunctionData function;
	
	/**
	 * Constructor for {@link QuadraticFunctionCommon}. Matrix and vector must have equals spans.
	 *
	 * @param aMatrix - matrix
	 * @param bVector - vector
	 * @param constant - constant
	 */
	public QuadraticFunctionCommon(final SquareMatrix aMatrix, final Vector bVector, final double constant) {
		this.span = aMatrix.size();
		if (bVector.size() != span) {
			throw new IllegalArgumentException("Matrix and vector must have equals span");
		}
		this.aMatrix = aMatrix;
		this.bVector = bVector;
		this.constant = constant;
	}
	
	public QuadraticFunctionCommon(final FunctionData function) {
		this.function = function;
	}
	
	/**
	 * Returns value of the function in given point.
	 *
	 * @param point - given point.
	 * @return - value of the function.
	 */
	@Override
	public Double apply(final Vector point) {
		if (function != null) {
			return function.apply(point);
		}
		
		double answer = 0;
		
		answer += aMatrix.mul(point).mul(point);
		
		answer /= 2;
		
		answer += bVector.mul(point);
		
		answer += constant;
		
		return answer;
	}
	
	/**
	 * Function returns gradient of Quadratic Function in given point. Point must have span equals to function span.
	 *
	 * @param point - {@link Vector}, characteristics of given point.
	 * @return - {@link Vector}, gradient of Quadratic function in given point.
	 */
	public Vector getGradient(final Vector point) {
		if (function != null) {
			return function.gradient(point);
		}
		return aMatrix.mul(point).sum(bVector);
	}
	
	/**
	 * Function returns span of matrix characterizing this function.
	 *
	 * @return - span of function.
	 */
	public int getSpan() {
		if (function != null) {
			return function.getSpan();
		}
		return span;
	}
	
	@Override
	public String toString() {
		return aMatrix + " * x^2 + " + bVector + " * x + " + String.format("%.4f", constant);
	}
}
