package skroba.lab2.methods;

import skroba.utils.Pair;
import skroba.utils.QuadraticFunctionCommon;
import skroba.utils.Vector;

/**
 * Simple realization of Conjugate gradient method, extended {@link AbstractGradientIterator}.
 */
public class ConjugateGradients extends AbstractGradientIterator{
	public final static String NAME = "Conjugate gradient method";
	
	private final int MAX_COUNT_OF_ITERATIONS;
	private int iteration;
	private Vector futureVector;
	
	/**
	 * Constructor with given EPS and given reset parameter.
	 * @param function - given minimizing function.
	 * @param EPS - double value characterizing precession of calculating.
	 * @param MAX_COUNT_OF_ITERATIONS - int value characterizing max count of iterations before reset.
	 */
	public ConjugateGradients(QuadraticFunctionCommon function, Double EPS, int MAX_COUNT_OF_ITERATIONS) {
		super(NAME, function, EPS);
		this.MAX_COUNT_OF_ITERATIONS = MAX_COUNT_OF_ITERATIONS;
		this.gradientNorm = Double.POSITIVE_INFINITY;
	}
	
	public ConjugateGradients(QuadraticFunctionCommon function, int MAX_COUNT_OF_ITERATIONS) {
		this(function, STANDARD_EPS, MAX_COUNT_OF_ITERATIONS);
	}
	
	@Override
	protected boolean hasNextPr() {
		checkIterations();
		
		if (comparator.compare(EPS, gradientNorm) != -1) {
			gradientNorm = Double.POSITIVE_INFINITY;
			iteration = 0;
			return false;
		}
		
		final Vector cPoint = currentValue.getFirst();
		final Vector tmp = function.aMatrix.mul(futureVector);
		final double alpha = gradientNorm * gradientNorm / tmp.mul(futureVector);
		final double prevGradientNorm = gradientNorm;
		final Vector fPoint = cPoint.sum(futureVector.scalarMul(alpha));
		nextValue = new Pair<>(fPoint, function.apply(fPoint));
		
		gradient = gradient.sum(tmp.scalarMul(alpha));
		gradientNorm = gradient.norma();
		final double beta = iteration == 0 ? 0 : ((gradientNorm * gradientNorm) / prevGradientNorm) / prevGradientNorm;
		futureVector = gradient.scalarMul(-1).sum(futureVector.scalarMul(beta));
		
		return true;
	}
	
	@Override
	protected Pair<Vector, Double> nextPr() {
		this.currentValue = this.nextValue;
		this.gradient = function.getGradient(currentValue.getFirst());
		this.gradientNorm = gradient.norma();
		return this.currentValue;
	}
	
	/**
	 * Method that increment count of iterations by module of MAX_COUNT_OF_ITERATIONS. If needed this function restart using parameters.
	 */
	private void checkIterations() {
		if (iteration == 0) {
			gradient = function.getGradient(currentValue.getFirst());
			gradientNorm = gradient.norma();
			futureVector = gradient.scalarMul(-1);
		}
		
		iteration += 1;
		iteration %= MAX_COUNT_OF_ITERATIONS;
	}
}
