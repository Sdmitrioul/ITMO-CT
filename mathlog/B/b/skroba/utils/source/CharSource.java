package b.skroba.utils.source;

import b.skroba.utils.exception.ParseException;

/**
 * Interface present char source.
 */
public interface CharSource {
	/**
	 * Returns true if source has more chars.
	 *
	 * @return true if source has another not chown char, false otherwise.
	 */
	boolean hasNext();
	
	/**
	 * Returns next char.
	 *
	 * @return next char in source.
	 */
	char next();
	
	/**
	 * Returns new {@link ParseException} at position in which source placed with given message.
	 *
	 * @param message - message from program using source.
	 * @return new instance of {@link ParseException}.
	 */
	ParseException error(final String message);
}
