package a.skroba.utils.exception;

/**
 * Class for exceptions appeared while parsing.
 */
public class ParseException extends RuntimeException {
	public ParseException(String message) {
		super(message);
	}
}
