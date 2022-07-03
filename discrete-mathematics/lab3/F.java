import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class F {
	private static final Integer MODULE = 1000000007;
	private static int k = 0, m = 0;
	private static long[] answer;
	private static int[] input;
	
	public static void main(String[] args) {
		try	(Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output();
		} catch (IOException e) {
			System.err.println("Problems with reading");
		}
	}
	
	private static void process() {
		long[] sum = new long[m + 1];
		answer = new long[m + 1];
		sum[0] = 1;
		answer[0] = 1;
		
		for (int i = 1; i <= m; i++) {
			for (int j = 0; j < k; j++) {
				if (i >= input[j]) {
					answer[i] += sum[i - input[j]];
					answer[i] %= MODULE;
				}
			}
			for (int j = 0; j <= i; j++) {
				sum[i] += (answer[j] * answer[i - j]) % MODULE;
				sum[i] %= MODULE;
			}
		}
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			for (int i = 1; i <= m; i++) {
				writer.write(answer[i] + " ");
			}
		} catch (IOException e) {
			System.err.println("Problems with output writing");
		}
	}
	
	private static void input(Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			k = scanner.getNextInt();
			m = scanner.getNextInt();
		}
		
		input = new int[k];
		
		if (scanner.hasLine()) {
			for (int i = 0; i < k; i++) {
				input[i] = scanner.getNextInt();
			}
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
