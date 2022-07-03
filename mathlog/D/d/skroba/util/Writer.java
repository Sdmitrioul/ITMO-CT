package d.skroba.util;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Writer implements AutoCloseable {
	private final BufferedWriter writer;
	
	public Writer() {
		this.writer = new BufferedWriter(new OutputStreamWriter(System.out));
	}
	
	public Writer(String filename) {
		Path outputPath = Paths.get(filename);
		try {
			if (outputPath.getParent() != null && !Files.exists(outputPath.getParent())) {
				Files.createDirectories(outputPath.getParent());
			}
			
			this.writer = Files.newBufferedWriter(outputPath);
		} catch (IOException ex) {
			throw new RuntimeException("Can't create parent directories");
		}
	}
	
	public void write(String output) {
		try {
			writer.write(output + '\n');
		} catch (IOException ex) {
			throw new RuntimeException("Can't write data");
		}
	}
	
	public void flush() {
		try {
			writer.flush();
		} catch (IOException ex) {
			throw new RuntimeException("Can't flush data");
		}
	}
	
	@Override
	public void close() {
		try {
			writer.close();
		} catch (IOException ex) {
			throw new RuntimeException("Can't close output stream");
		}
	}
}
