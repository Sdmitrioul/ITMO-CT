package skroba.utils;

/**
 * Simple class, containing pair of elements.
 * @param <F> - first element.
 * @param <S> - second element.
 */
public class Pair<F, S> {
	final F first;
	final S second;
	
	public Pair(F first, S second) {
		this.first = first;
		this.second = second;
	}
	
	/**
	 * Returns first value.
	 * @return {@link F} first parameter of pair.
	 */
	public F getFirst() {
		return first;
	}
	
	/**
	 * Returns second value.
	 * @return {@link S} second parameter of pair.
	 */
	public S getSecond() {
		return second;
	}
	
	@Override
	public String toString() {
		return "<" + first + ", " + second + ">";
	}
}
