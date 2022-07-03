package skroba.lab2.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.methods.GoldenRatioMethod;
import skroba.lab1.methods.MinimumSearcher;
import skroba.utils.Pair;
import skroba.utils.QuadraticFunctionCommon;
import skroba.utils.Vector;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.function.Function;

/**
 * Simple realization of Fast Gradient Method, extended {@link AbstractGradientIterator}.
 */
public class FastGradientMethod extends AbstractGradientIterator {
	private final static String NAME = "Fast gradient descent";
	private final static Class<? extends MinimumSearcher> ONE_DIM_STANDARD_MINIMUM_SEARCHER = GoldenRatioMethod.class;
	private Constructor<? extends MinimumSearcher> oneDimSearcherConstructor;
	
	/**
	 *	Constructor with standard EPS equals 1e-3.
	 *
	 * @param function - given minimizing function.
	 */
	public FastGradientMethod(final QuadraticFunctionCommon function) {
		this(function, ONE_DIM_STANDARD_MINIMUM_SEARCHER);
	}
	
	/**
	 * Constructor with given EPS.
	 *
	 * @param function - given minimizing function.
	 * @param EPS - double value characterizing precession of calculating.
	 */
	public FastGradientMethod(final QuadraticFunctionCommon function, final Double EPS) {
		this(function, EPS, ONE_DIM_STANDARD_MINIMUM_SEARCHER);
	}
	
	/**
	 * Constructor with given EPS and one dim minimum searcher.
	 *
	 * @param function - given minimizing function.
	 * @param EPS - double value characterizing precession of calculating.
	 * @param oneDimSearcherToken - {@link Class} token of minimum searcher. It must implement {@link MinimumSearcher} interface.
	 */
	public FastGradientMethod(final QuadraticFunctionCommon function, final Double EPS, final Class<? extends MinimumSearcher> oneDimSearcherToken) {
		super(NAME, function, EPS);
		setConstructor(oneDimSearcherToken);
	}
	
	/**
	 * Constructor with given one dim minimum searcher.
	 *
	 * @param function - given minimizing function.
	 * @param oneDimSearcherToken - {@link Class} token of minimum searcher. It must implement {@link MinimumSearcher} interface.
	 */
	public FastGradientMethod(final QuadraticFunctionCommon function, final Class<? extends MinimumSearcher> oneDimSearcherToken) {
		this(function, STANDARD_EPS, oneDimSearcherToken);
	}
	
	@Override
	protected boolean hasNextPr() {
		final Vector cPoint = currentValue.getFirst();
		
		if (comparator.compare(EPS, gradientNorm) != -1) {
			return false;
		}
		
		final Function<Double, Vector> oneDimFun = x -> cPoint.sum(gradient.scalarMul(-x));
		
		MinimumSearcher searcher = getMinimumSearcher(oneDimFun);
		
		try {
			final double val = searcher.findMin(0.0, 1.0).getMin();
			final Vector result = oneDimFun.apply(val);
			nextValue = new Pair<>(result, function.apply(result));
			return true;
		} catch (TimeOutException ex) {
			return false;
		}
	}
	
	@Override
	protected Pair<Vector, Double> nextPr() {
		this.currentValue = this.nextValue;
		this.gradient = function.getGradient(currentValue.getFirst());
		this.gradientNorm = gradient.norma();
		return this.currentValue;
	}
	
	/**
	 * Function that get constructor of one dim minimum searcher {@link MinimumSearcher}. It should be called only in class constructor.
	 *
	 * @param oneDimSearcherToken - {@link Class} token implements {@link MinimumSearcher}.
	 */
	private void setConstructor(final Class<? extends MinimumSearcher> oneDimSearcherToken) {
		try {
			oneDimSearcherConstructor = oneDimSearcherToken.getConstructor(double.class, double.class, Function.class);
		} catch (NoSuchMethodException ex) {
			throw new RuntimeException("Can't get constructor from one dim class: " + ex.getMessage());
		}
	}
	
	/**
	 * Function that returns new instance of minimum searcher.
	 * @param oneDimFun - one dim function for minimizing.
	 * @return - instance of class implementing {@link MinimumSearcher} interface.
	 */
	private MinimumSearcher getMinimumSearcher(Function<Double, Vector> oneDimFun) {
		try {
			return oneDimSearcherConstructor.newInstance(EPS / 100, STANDARD_DELTA, oneDimFun.andThen(function));
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException ex) {
			throw new RuntimeException("There's no such constructor: " + ex.getMessage());
		}
	}
}
