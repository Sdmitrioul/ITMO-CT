import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.NoSuchElementException;

public class A {
	private static final int MODULE = 998_244_353;
	private static int n = 0, m = 0;
	private static HashMap<Integer, Long> P, Q, Added, Multi;
	private static HashMap<Integer, Long> Div = new HashMap<>(1000);
	
	public static void main(String[] args) {
		try  (Scanner scanner = new Scanner(System.in)) {
			read(scanner);
			addPolynomics();
			multiPolynomics();
			dividePolynomics();
			output();
		} catch (IOException e) {
			System.err.println("Reading problem: " + e.getMessage());
		}
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			write(writer, Added, true);
			write(writer, Multi, true);
			write(writer, Div, false);
		} catch (IOException e) {
			System.err.println("Problems with output writing");
		}
	}
	
	private static void write(final BufferedWriter writer, final HashMap<Integer, Long> pol, boolean log) throws IOException {
		if (log) {
			writer.write(Integer.toString(pol.size() - 1));
			writer.newLine();
		}
		for (int i = 0; i < pol.size(); i++) {
			writer.write(pol.get(i) + " ");
		}
		writer.newLine();
	}
	
	private static void dividePolynomics() {
		for (int i = 0; i < 1000; i++) {
			long sum = 0;
			for (int j = 0; j < i; j++) {
				sum = sum(sum, mul(Div.getOrDefault(j, 0L), Q.getOrDefault(i - j, 0L)), MODULE);
			}
			Div.put(i, sum(sum(P.getOrDefault(i, 0L), -sum) / Q.get(0), MODULE));
		}
	}
	
	private static void multiPolynomics() {
		Multi = new HashMap<>(n + m + 1);
		for (int i = 0; i <= n; i++) {
			final long index = P.get(i);
			for (int j = 0; j <= m; j++) {
				Multi.put(i + j, sum(Multi.getOrDefault(i + j, 0L), mul(index, Q.get(j))));
			}
		}
	}
	
	private static void addPolynomics() {
		Added = new HashMap<>(Math.max(n, m));
		for (int i = 0; i <= Math.max(n, m); i++) {
			Added.put(i, sum(P.getOrDefault(i, 0L), Q.getOrDefault(i, 0L)));
		}
	}
	
	private static void read(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
			m = scanner.getNextInt();
		}
		P = readPolinom(scanner, n);
		Q = readPolinom(scanner, m);
	}
	
	private static HashMap<Integer, Long> readPolinom(final Scanner scanner, final int size) throws IOException {
		HashMap<Integer, Long> polynomial = new HashMap<>(size);
		if (scanner.hasLine()) {
			for (int i = 0; i <= size; i++) {
				polynomial.put(i, (long) scanner.getNextInt());
			}
		}
		return polynomial;
	}
	
	private static long sum(long ... args) {
		long sum = 0;
		for (long i : args) {
			sum = (sum + i) % MODULE;
		}
		return sum;
	}
	
	private static long mul(long ... args) {
		long mul = 1;
		for (long i : args) {
			mul = mul * i % MODULE;
		}
		return mul;
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
