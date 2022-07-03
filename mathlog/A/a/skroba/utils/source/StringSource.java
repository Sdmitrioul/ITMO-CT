package a.skroba.utils.source;

import a.skroba.utils.exception.ParseException;

/**
 * Class implements interface {@link CharSource}. Realization based on String.
 */
public final class StringSource implements CharSource {
	private final String source;
	private int pos;
	
	public StringSource(final String source) {
		this.source = source;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean hasNext() {
		return pos < source.length();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public char next() {
		return source.charAt(pos++);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public ParseException error(String message) {
		return new ParseException(String.format(
				"Exception at pos <%d>: message - %s",
				pos,
				message)
		);
	}
}
