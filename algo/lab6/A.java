import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

public class A {
	private static int number;
	private final static List<Integer> answer = new ArrayList<>();
	
	public static void main(String[] args) {
		try (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output();
		} catch (IOException ex) {
			System.err.println("Exception while reading: " + ex.getMessage());
		}
	}
	
	private static void output() {
		try	(BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(answer.stream().map(x -> Integer.toString(x)).collect(Collectors.joining(" ")));
		} catch (IOException ex) {
			System.err.println("Exception while writing: " + ex.getMessage());
		}
	}
	
	private static void process() {
		int curNumber = number;
		
		int i = 2;
		while (i <= Math.sqrt(number) && curNumber > 1) {
			if (curNumber % i != 0) {
				i++;
				continue;
			}
			
			curNumber /= i;
			answer.add(i);
		}
		
		if (curNumber > 1) {
			answer.add(curNumber);
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			number = scanner.getNextInt();
		}
	}
	
	private static class Scanner implements AutoCloseable {
		private BufferedReader reader;
		private String line;
		private boolean readed;
		private int index = 0;
		private int position = 0;
		private String mark;
		
		public Scanner(InputStream in) throws UnsupportedEncodingException {
			try{
				reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8.name()));
			} catch (UnsupportedEncodingException e){
				throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
			}
		}
		
		public Scanner(String s) throws FileNotFoundException, UnsupportedEncodingException {
			try{
				reader = new BufferedReader(new InputStreamReader(new FileInputStream(s), StandardCharsets.UTF_8.name()));
			} catch (FileNotFoundException e){
				throw new FileNotFoundException("File not found :" + e.getMessage());
			} catch (UnsupportedEncodingException e){
				throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
			}
		}
		
		public boolean hasNextInt() {
			while (index < line.length() && Character.isWhitespace(line.charAt(index))){
				index++;
			}
			return index != line.length();
		}
		
		public boolean hasNextWord() {
			while (index < line.length() && !isWordSymbol(line.charAt(index))){
				//System.out.println(line.charAt(index) + " " + Character.isLetter(line.charAt(index)));
				index++;
			}
			return index != line.length();
		}
		
		private boolean isWordSymbol(char c) {
			return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION ||  c == '\'';
		}
		
		public int getNextInt() {
			if (!hasNextInt()) {
				throw new NoSuchElementException();
			}
			int begin = index;
			while (!Character.isWhitespace(line.charAt(index))){
				index++;
				if (index == line.length()){
					break;
				}
			}
			return Integer.parseInt(line.substring(begin, index));
		}
		
		public String getNextWord() {
			if (!hasNextWord()) {
				throw new NoSuchElementException();
			}
			int begin = index;
			while (isWordSymbol(line.charAt(index))){
				index++;
				if (index == line.length()){
					break;
				}
			}
			return line.substring(begin, index);
		}
		
		public boolean hasLine() throws IOException {
			try{
				index = 0;
				readed = true;
				line = reader.readLine();
			} catch (IOException e) {
				throw new IOException("Input error in Scanner.hasLine(): " + e.getMessage());
			}
			return line != null;
		}
		
		public String getLine() throws IOException {
			if (readed) {
				readed = false;
				return line;
			} else {
				if (hasLine()) {
					readed = false;
					return line;
				} else {
					return "";
				}
			}
		}
		
		public void close() throws IOException {
			try{
				reader.close();
			} catch(IOException e) {
				throw new IOException("Eror: has problem in scanner.close() " + e.getMessage());
			}
		}
	}
}
