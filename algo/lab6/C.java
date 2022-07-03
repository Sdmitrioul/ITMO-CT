import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class C {
	private static final String READING_PROBLEM = "Exception, while reading: ";
	private static final String WRITING_PROBLEM = "Exception, while writing in buffer: ";

	public static void main(String[] args) {
		new C().run();
	}
	
	public void run() {
		try (final Scanner scanner = new Scanner(System.in)) {
			long[] input = input(scanner);
			calculate(input);
			output(input[0]);
		} catch (IOException ex) {
			System.err.println(READING_PROBLEM);
		}
	}
	
	private void calculate(final long[] input) {
		long ma = input[2] * input[3]; //First stp
		
		long m1 = ma / input[2]; //Second step
		long m2 = ma / input[3];
		
		long rm1 = getReverseNumber(m1, input[2]); // Third step
		long rm2 = getReverseNumber(m2, input[3]);
		
		long x = (input[0] * rm1 * m1 + input[1] * rm2 * m2) % ma;
		
		long ans = x;
		
		while (x >= 0) {
			x -= ma;
			
			if (x >= 0) {
				ans = x;
			}
		}
		
		input[0] = ans;
	}
	
	public long getReverseNumber(long a, long mod) {
		Pair pair = new Pair();
		long g = gcd(a, mod, pair);
		if (g != 1) {
			throw new RuntimeException("Can't find reversed number");
		}
		
		return (pair.a % mod + mod) % mod;
	}
	
	public long[] input(final Scanner scanner) throws IOException {
		long[] input = new long[4];
		
		if (scanner.hasLine()) {
			for (int i = 0; i < 4; i++) {
				input[i] = scanner.getNextInt();
			}
		}
		
		return input;
	}
	
	public void output(final long ans) {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(ans + " ");
		} catch (IOException ex) {
			System.err.println(WRITING_PROBLEM + ex.getMessage());
		}
	}
	
	private long gcd(long a, long b, Pair pair) {
		if (a == 0) {
			pair.a = 0;
			pair.b = 1;
			return b;
		}
		
		Pair nPair = new Pair();
		long d = gcd(b%a, a, nPair);
		pair.a = nPair.b - (b / a) * nPair.a;
		pair.b = nPair.a;
		return d;
	}
	
	private static class Pair {
		long a = Integer.MIN_VALUE;
		long b = Integer.MIN_VALUE;;
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
