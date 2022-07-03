package skroba.lab2.methods;

import skroba.utils.Pair;
import skroba.utils.QuadraticFunctionCommon;
import skroba.utils.Vector;

/**
 * Simple realization of Gradient Method, extended {@link AbstractGradientIterator}.
 */
public class GradientMethod extends AbstractGradientIterator {
	private final static String NAME = "Gradient descent";
	private double step = 1.0;
	
	/**
	 *	Constructor with standard EPS equals 1e-3.
	 *
	 * @param function - given minimizing function.
	 */
	public GradientMethod(final QuadraticFunctionCommon function) {
		super(NAME, function);
	}
	
	/**
	 * Constructor with given EPS.
	 *
	 * @param function - given minimizing function.
	 * @param EPS - double value characterizing precession of calculating.
	 */
	protected GradientMethod(final QuadraticFunctionCommon function, final Double EPS) {
		super(NAME, function, EPS);
	}
	
	@Override
	protected boolean hasNextPr() {
		final Vector cPoint = currentValue.getFirst();
		
		if (comparator.compare(EPS, gradientNorm) != -1) {
			return false;
		}
		
		final Vector nPoint = cPoint
				.sum(gradient.scalarMul(- step / gradientNorm));
		
		final double nValue = function.apply(nPoint);
		
		if (nValue < currentValue.getSecond()) {
			nextValue = new Pair<>(nPoint, nValue);
			return true;
		}
		
		step /= 2;
		
		return hasNext();
	}
	
	@Override
	protected Pair<Vector, Double> nextPr() {
		this.currentValue = this.nextValue;
		this.gradient = function.getGradient(currentValue.getFirst());
		this.gradientNorm = gradient.norma();
		return this.currentValue;
	}
}
