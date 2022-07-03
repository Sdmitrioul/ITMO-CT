package skroba.utils.logger;

import skroba.utils.fileWriter.FileWriter;
import skroba.utils.fileWriter.FileWriterImpl;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Implementation of {@link Logger}.
 */
public class LoggerImpl implements Logger {
	private boolean log = true;
	private final FileWriter file;
	
	public LoggerImpl(final String fileName, final boolean log) {
		this.log = log;
		file = new FileWriterImpl(fileName);
	}
	
	public LoggerImpl(final Path path, final boolean log) {
		this.log = log;
		file = new FileWriterImpl(path);
	}
	
	public LoggerImpl(final String fileName) {
		this(fileName, true);
	}
	
	public LoggerImpl(final Path path) {
		this(path, true);
	}
	
	
	/**
	 * Set needing logging parameter.
	 * @param log - if need log.
	 */
	public void setLog(boolean log) {
		this.log = log;
	}
	
	@Override
	public void log(final String message) {
		if (!log) return;
		file.write(message.replaceAll(",", "."));
	}
	
	@Override
	public void close() {
		file.flush();
		file.close();
	}
}
