package a.skroba.utils;

import a.skroba.utils.exception.ParseException;
import a.skroba.utils.source.CharSource;

/**
 * Abstract class for parser.
 */
public abstract class AbstractParser {
	public static final char END = '\0';
	private final CharSource source;
	protected char ch = 0xffff;
	
	public AbstractParser(CharSource source) {
		this.source = source;
	}
	
	/**
	 * Assign next char from source to ch, if source hasn't another chars give to it {@code '\0'}.
	 */
	protected void nextChar() {
		ch = source.hasNext() ? source.next() : END;
	}
	
	/**
	 * Returns true if ch equals to expected.
	 * @param expected - char that is expected to be.
	 * @return true or false.
	 */
	protected boolean test(final char expected) {
		if (ch == expected) {
			nextChar();
			return true;
		}
		
		return false;
	}
	
	/**
	 * if next char in source not equals to expected this method throw an exception.
	 * @param c - expected char symbol.
	 */
	protected void expect(final char c) {
		if (ch != c) {
			throw error("Expected '" + c + "', found '" + ch + "'");
		}
		nextChar();
	}
	
	/**
	 * if next chars in source not equals to expected string this method throw an exception.
	 * @param value - expected string.
	 */
	protected void expect(final String value) {
		for (char c : value.toCharArray()) {
			expect(c);
		}
	}
	
	/**
	 * Check if it is end of source.
	 * @return - true if source ended.
	 */
	protected boolean eof() {
		return test(END);
	}
	
	/**
	 * Return exception.
	 * @param message - exception message.
	 * @return - new Instance of {@link ParseException}
	 */
	protected ParseException error(final String message) {
		return source.error(message);
	}
	
	/**
	 * Checks if current char is between given.
	 * @param from - smallest char.
	 * @param to - biggest char.
	 * @return - true if between, otherwise false.
	 */
	protected boolean between(final char from, final char to) {
		return from <= ch && ch <= to;
	}
	
	/**
	 * Checks if current char is equal with given.
	 * @param expected - expected character.
	 * @return - returns true if expected and current symbol equals, false otherwise.
	 */
	protected boolean check(final char expected) {
		return expected == ch;
	}
	
	protected void log(final String message) {
		System.out.println(message);
	}
}
