package skroba.lab2.methods;

import skroba.utils.*;
import skroba.utils.logger.GradientLogger;
import skroba.utils.logger.Logger;

import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Abstract class for gradient methods.
 */
public abstract class AbstractGradientIterator implements Iterator<Pair<Vector, Double>> {
	protected static final Double STANDARD_EPS = 1e-3;
	protected static final Double STANDARD_DELTA = 0.95;
	protected final Double EPS;
	protected final QuadraticFunctionCommon function;
	private final String methodName;
	private GradientLogger logger;
	protected final DoubleComparator comparator;
	protected Pair<Vector, Double> currentValue;
	protected Pair<Vector, Double> nextValue;
	protected Vector gradient;
	protected double gradientNorm;
	
	public AbstractGradientIterator(final String methodName, final QuadraticFunctionCommon function, final Double EPS) {
		this.function = function;
		this.methodName = methodName;
		this.EPS = EPS;
		this.comparator = new DoubleComparator(EPS);
		final Vector nVector = new Vector(Collections.nCopies(function.getSpan(), 1.0));
		this.currentValue = new Pair<>(nVector, function.apply(nVector));
		this.gradient = function.getGradient(nVector);
		this.gradientNorm = gradient.norma();
	}
	
	public AbstractGradientIterator(final String methodName, final QuadraticFunctionCommon function) {
		this(methodName, function, STANDARD_EPS);
	}
	
	@Override
	public boolean hasNext() {
		return hasNextPr();
	}
	
	@Override
	public Pair<Vector, Double> next() {
		if (currentValue == nextValue && !hasNext()) {
			throw new NoSuchElementException("There isn't any next position");
		}
		var next = nextPr();
		logState();
		return next;
	}
	
	protected abstract boolean hasNextPr();
	
	protected abstract Pair<Vector, Double> nextPr();
	
	private void logState() {
		if (logger != null) {
			logger.log(currentValue);
		}
	}
	
	/**
	 * Set logger.
	 *
	 * @param logger - logger, class have {@link Logger} interface.
	 */
	public void setLogger(final GradientLogger logger) {
		this.logger = logger;
		logState();
	}
	
	/**
	 * Returns log data.
	 *
	 * @return data
	 */
	public List<Pair<Vector, Double>> getLogs() {
		return logger.getData();
	}
	
	/**
	 * Function that returns method name.
	 * @return - {@link String}, presenting method name.
	 */
	public String getMethodName() {
		return methodName;
	}
	
	/**
	 * Returns current value of iterator.
	 *
	 * @return {@link Pair} of {@link Vector} showing current point and double value, result of function on current point.
	 */
	public Pair<Vector, Double> getCurrentValue() {
		return currentValue;
	}
	
	/**
	 * Gradient of the function in current point.
	 * @return {@link Vector} gradient in current point.
	 */
	public Vector getGradient() {
		return function.getGradient(currentValue.getFirst());
	}
	
	/**
	 * Returns iterating function.
	 * @return {@link QuadraticFunctionCommon} function.
	 */
	public QuadraticFunctionCommon getFunction() {
		return function;
	}
}
