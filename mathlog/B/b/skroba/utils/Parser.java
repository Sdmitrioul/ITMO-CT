package b.skroba.utils;

import b.skroba.utils.source.CharSource;

/**
 * Parser interface.
 * @param <T> - returning type.
 */
public interface Parser<T> {
	T parse(CharSource source);
}
