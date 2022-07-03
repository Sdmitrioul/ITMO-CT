package c.skroba.parser.source;

import c.skroba.parser.ParseException;

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
	 * Returns true if source has previous chars.
	 *
	 * @return true if source has another not chown char, false otherwise.
	 */
	boolean hasPrev();
	
	/**
	 * Returns next char.
	 *
	 * @return next char in source.
	 */
	char next();
	
	/**
	 * Returns previous char.
	 *
	 * @return previous char in source.
	 */
	char prev();
	
	/**
	 * Returns current char.
	 *
	 * @return previous char in source.
	 */
	char current();
	
	/**
	 * Returns new {@link ParseException} at position in which source placed with given message.
	 *
	 * @param message - message from program using source.
	 * @return new instance of {@link ParseException}.
	 */
	ParseException error(final String message);
}
