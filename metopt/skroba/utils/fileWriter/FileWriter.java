package skroba.utils.fileWriter;

import java.io.Closeable;

/**
 * Simple interface fo writing in file.
 */
public interface FileWriter extends Closeable {
	void write(String message);
	void flush();
	void close();
}
