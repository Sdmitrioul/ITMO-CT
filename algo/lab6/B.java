import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

public class B {
	public static void main(String[] args) {
		new B().run();
	}
	
	private static final String READING_PROBLEM = "Exception, while reading: ";
	private static final String WRITING_PROBLEM = "Exception, while writing in buffer: ";
	private static final int COUNT_OF_ROUNDS = 3;
	
	public void run() {
		try (final Scanner scanner = new Scanner(System.in)) {
			long[] input = input(scanner);
			algorithm(input);
			output(input);
		} catch (IOException ex) {
			System.err.println(READING_PROBLEM + ex.getMessage());
		}
	}
	
	private void algorithm(final long[] input) {
		for (int i = 0; i < input.length; i++) {
			input[i] = isPrime(input[i]) ? 1 : 0;
		}
	}
	
	public long[] input(final Scanner scanner) throws IOException {
		int n = 0;
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
		}
		
		long[] input = new long[n];
		
		for (int i = 0; i < n; i++) {
			if (scanner.hasLine()) {
				input[i] = scanner.getNextLong();
			}
		}
		
		return input;
	}
	
	public void output(final long[] ans) {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(
					Arrays.stream(ans)
							.mapToObj(x -> x == 1 ? "YES" : "NO")
							.collect(Collectors.joining("\n"))
			);
		} catch (IOException ex) {
			System.err.println(WRITING_PROBLEM + ex.getMessage());
		}
	}
	
	private boolean isPrime(final long n) {
		if (n == 2 || n == 3 || n == 5) {
			return true;
		}
		
		if (n == 1 || n % 2 == 0) {
			return false;
		}
		
		long p = 0;
		long q = n - 1;
		
		while (q % 2 == 0) {
			p++;
			q >>= 1;
		}
		
		for (int i = 0; i < COUNT_OF_ROUNDS; i++) {
			long a = (long) (((Math.random() * (n - 2)) % (n - 2)) + 2);
			long x = pow(a, q, n);
			
			if (x == 1 || x == n - 1) {
				continue;
			}
			
			for (int j = 1; j < p; j++) {
				x = multiply(x, x, n);
				if (x == 1) {
					return false;
				}
				if (x == n - 1) {
					break;
				}
			}
			
			if (x != n - 1) {
				return false;
			}
		}
		
		return true;
	}
	
	public long pow(long a, long q, long n) {
		long result = 1;
		while (q > 0) {
			if ((q & 1) == 1) {
				result = multiply(result, a, n);
			}
			a = multiply(a, a, n);
			q >>= 1;
		}
		return result;
	}
	
	public long multiply(long a, long n, long m) {
		long result = 0;
		while (n > 0) {
			if (n % 2 == 1) {
				result = (result + a) % m;
			}
			a = (a + a) % m;
			n >>= 1;
		}
		return result;
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
		
		public long getNextLong() {
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
			return Long.parseLong(line.substring(begin, index));
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
