package skroba.utils.logger;

import skroba.utils.Pair;

/**
 * Simple logger interface.
 */
public interface Logger {
	void log(String message);
	void close();
}
