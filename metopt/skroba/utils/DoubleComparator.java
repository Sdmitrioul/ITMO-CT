package skroba.utils;

import java.util.Comparator;

/**
 * Simple realization of {@link Double} comparator.
 */
public class DoubleComparator implements Comparator<Double> {
	private final double EPS;
	
	/**
	 * Returns comparator of double values.
	 * @param EPS - estimated faulty proportion.
	 */
	public DoubleComparator(double EPS) {
		this.EPS = EPS;
	}
	
	@Override
	public int compare(final Double first, final Double second) {
		if (Math.abs(first - second) <= EPS) {
			return 0;
		}
		
		if (first - EPS > second) {
			return 1;
		}
		
		return -1;
	}
}
