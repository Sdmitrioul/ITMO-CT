package skroba.utils.fileWriter;

import skroba.exceptions.FileWriterException;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Simple implementation of {@link FileWriter}.
 */
public class FileWriterImpl implements FileWriter {
	private final BufferedWriter writer;
	
	/**
	 * Constructor from file name. If there isn't needed directory it creates it.
	 * @param filename - filename.
	 */
	public FileWriterImpl(String filename) {
		this(Paths.get(filename));
	}
	
	public FileWriterImpl(Path path) {
		try {
			if (path.getParent() != null && !Files.exists(path.getParent())) {
				Files.createDirectories(path.getParent());
			}
		} catch (IOException ex) {
			throw new FileWriterException("Can't create directory for output writing: " + ex.getMessage());
		}
		
		try {
			writer = Files.newBufferedWriter(path);
		} catch (IOException ex) {
			throw new FileWriterException("Can't open BufferedWriter: " + ex.getMessage());
		}
	}
	
	
	@Override
	public void write(final String message) {
		try {
			writer.write(message);
		} catch (IOException ex) {
			throw new FileWriterException("Can't write message - <" + message + "> in file: " + ex.getMessage());
		}
	}
	
	@Override
	public void flush() {
		try {
			writer.flush();
		} catch (IOException ex) {
			throw new FileWriterException("Can't flush data: " + ex.getMessage());
		}
	}
	
	@Override
	public void close() {
		try {
			writer.close();
		} catch (IOException ex) {
			throw new FileWriterException("Can't close fileWriter: " + ex.getMessage());
		}
	}
}
