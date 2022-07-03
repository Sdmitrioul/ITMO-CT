package a.skroba.utils;

import a.skroba.utils.source.CharSource;

/**
 * Parser interface.
 * @param <T> - returning type.
 */
public interface Parser<T> {
	T parse(CharSource source);
}
