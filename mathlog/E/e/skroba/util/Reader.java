package e.skroba.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Reader implements AutoCloseable {
	private BufferedReader reader;
	private String nextString;
	private boolean shown;
	
	public Reader() {
		this.reader = new BufferedReader(new InputStreamReader(System.in));
	}
	
	public Reader(String filename) {
		try {
			this.reader = Files.newBufferedReader(Paths.get(filename));
		} catch (IOException ex) {
			throw new RuntimeException("Can't read file");
		}
	}
	
	public String next() {
		if (shown) {
			return hasNext() ? next() : null;
		}
		shown = true;
		return nextString;
	}
	
	public boolean hasNext() {
		try {
			nextString = reader.readLine();
			shown = false;
			return !(nextString.isEmpty() || nextString.isBlank() || nextString == null);
		} catch (IOException ex) {
			throw new RuntimeException("Can't read next line");
		}
	}
	
	@Override
	public void close() {
		try {
			reader.close();
		} catch (IOException ex) {
			throw new RuntimeException("Can't close input stream");
		}
	}
}
