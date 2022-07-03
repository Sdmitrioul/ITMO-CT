import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class C {
	private static int k = 0;
	private static final Integer MODULE = 1000000007;
	private static long[] a, c, p, q;
	
	public static void main(String[] args) {
		try (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output(p, q);
		} catch (IOException e) {
			System.err.println("Reading file problem");
		}
	}
	
	private static void process() {
		q = new long[k + 1];
		q[0] = 1;
		for (int i = 1; i <= k; i++) {
			q[i] = -c[i - 1];
		}
		p = new long[k];
		System.arraycopy(multi(a, q), 0, p, 0, k);
	}
	
	private static long[] multi(final long[] a, final long[] q) {
		long[] p = new long[a.length + q.length + 1];
		for (int i = 0; i < a.length; i++) {
			final long el = a[i];
			for (int j = 0; j < q.length; j++) {
				p[i + j] += el * q[j];
			}
		}
		return p;
	}
	
	private static void output(final long[] ... args) {
		try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			for (long[] pol : args) {
				writePol(writer, pol, findPos(pol));
				writer.newLine();
			}
		} catch (IOException e) {
			System.err.println("Writing out problem");
		}
	}
	
	private static int findPos(final long[] pol) {
		int i = pol.length - 1;
		while (i > 0 && pol[i] == 0) {
			i--;
		}
		return i + 1;
	}
	
	private static void writePol(final BufferedWriter writer, final long[] pol, final int n) throws IOException {
		writer.write(n - 1 + "\n");
		for (int i = 0; i < n; i++) {
			writer.write(pol[i] + " ");
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			k = scanner.getNextInt();
		}
		
		a = new long[k];
		c = new long[k];
		
		if (scanner.hasLine()) {
			for (int i = 0; i < k; i++) {
				a[i] = scanner.getNextInt();
			}
		}
		
		if (scanner.hasLine()) {
			for (int i = 0; i < k; i++) {
				c[i] = scanner.getNextInt();
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
