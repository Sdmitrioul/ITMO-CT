package skroba.utils;

/**
 * Simple Iterator interface.
 * @param <T>
 */
public interface Iterator<T> {
	boolean hasNext();
	T next();
}
