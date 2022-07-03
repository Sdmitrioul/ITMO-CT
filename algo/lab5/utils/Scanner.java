package utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.NoSuchElementException;

public class Scanner implements AutoCloseable {
	private final BufferedReader reader;
	private String currentInput;
	private String delimiter = " ";
	private int index = 0;
	private boolean alreadyRead = false;
	private String[] lines;
	
	public Scanner() {
		reader = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
	}
	
	public Scanner(Path filePath) throws IOException {
		reader = Files.newBufferedReader(filePath);
	}
	
	public Scanner(String fileName) throws IOException {
		this(Paths.get(fileName));
	}
	
	public String delimiter() {
		return this.delimiter;
	}
	
	public void useDelimiter(String delimiter) {
		this.delimiter = delimiter;
	}
	
	public boolean hasNextLine() throws IOException {
		alreadyRead = false;
		
		currentInput = reader.readLine();
		
		if (currentInput != null) {
			lines = currentInput.split(delimiter);
		}
		
		return currentInput != null;
	}
	
	public String nextLine() throws IOException {
		currentInput = alreadyRead ? reader.readLine() : currentInput;
		
		alreadyRead = true;
		
		if (currentInput == null) {
			throw new NullPointerException();
		}
		
		return currentInput;
	}
	
	public boolean hasNext() {
		return !alreadyRead && index < lines.length;
	}
	
	public Object next() {
		return nextWord();
	}
	
	public boolean hasNextWord() {
		return hasNext();
	}
	
	public String nextWord() {
		if (index >= lines.length) {
			throw new NoSuchElementException();
		}
		return lines[index++];
	}
	
	public boolean hasNextInt() {
		try {
			int newInt = Integer.parseInt(lines[index]);
		} catch (NumberFormatException e) {
			//ignore
			return false;
		}
		return true;
	}
	
	public int nextInt() {
		return Integer.parseInt(lines[index++]);
	}
	
	public boolean hasNextLong() {
		try {
			long newInt = Long.parseLong(lines[index]);
		} catch (NumberFormatException e) {
			//ignore
			return false;
		}
		return true;
	}
	
	public long nextLong() {
		return Long.parseLong(lines[index++]);
	}
	
	
	
	@Override
	public void close() throws IOException {
		reader.close();
	}
}
