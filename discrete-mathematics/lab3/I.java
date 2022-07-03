import java.io.*;
import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class I {
	private final static int MODULE = 104_857_601;
	private static int k = 0;
	private static long n = 0;
	private static Polynom c, a;
	
	public static void main(final String[] args) {
		try (final Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output();
		} catch (IOException e) {
			System.err.println("Reading file problem");
		}
	}
	
	private static void process() {
		while (n >= k) {
			for (int i = k; i < 2 * k; i++) {
				long sum = 0;
				for (int j = 1; j <= k; j++) {
					sum += -c.getOrDefault(j, 0) * a.getOrDefault(i - j, 0) % MODULE;
					sum %= MODULE;
					if (sum < 0) {
						sum += MODULE;
					}
				}
				a.set(i, sum);
			}
			Polynom r = c.mul(c.getMul(-1));
			a.filter(n);
			r.filter(0);
			c = r;
			n /= 2;
		}
	}
	
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(a.getOrDefault((int) n, -54) + " ");
		} catch (IOException e) {
			System.err.println("Writing problem: " + e.getMessage());
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			k = scanner.getNextInt();
			n = scanner.getNextLong() - 1;
		}
		
		if (scanner.hasLine()) {
			long[] am = new long[k * 2];
			for (int i = 0; i < k; i++) {
				am[i] = scanner.getNextInt();
			}
			a = new Polynom(am, MODULE);
		}
		
		if (scanner.hasLine()) {
			long[] cm = new long[k + 1];
			for (int i = 1; i <= k; i++) {
				cm[i] = (MODULE - scanner.getNextInt()) % MODULE;
			}
			cm[0] = 1;
			c = new Polynom(cm, MODULE);
		}
	}
	
	private static class Polynom {
		private long[] polynom;
		private long MODULE = Long.MAX_VALUE;
		
		public Polynom(long[] polynom) {
			this.polynom = polynom;
		}
		
		public Polynom(long[] polynom, long module) {
			this.polynom = polynom;
			this.MODULE = module;
		}
		
		public Polynom(long el, int size) {
			polynom = new long[size];
			polynom[0] = el;
		}
		
		public long getOrDefault(int pos, long defaultValue) {
			return pos < polynom.length ? polynom[pos] : defaultValue;
		}
		
		public int size() {
			return polynom.length;
		}
		
		public Polynom mul(Polynom pol) {
			long[] multi = new long[pol.size() + this.size() + 1];
			for (int i = 0; i < pol.size(); i++) {
				long el = pol.getOrDefault(i, 0);
				for (int j = 0; j < this.size(); j++) {
					multi[i + j] += el * polynom[j];
					multi[i + j] %= MODULE;
				}
			}
			return new Polynom(multi, MODULE);
		}
		
		public void mul(long k) {
			for (int i = 1; i < size(); i++) {
				if (i % 2 == 1) {
					polynom[i] = polynom[i] * k % MODULE;
				}
			}
		}
		
		public Polynom getMul(long k) {
			long[] pol = new long[size()];
			pol[0] = polynom[0];
			for (int i = 1; i < size(); i++) {
				if (i % 2 == 1) {
					pol[i] = (-polynom[i] + MODULE) % MODULE;
				} else {
					pol[i] = polynom[i];
				}
			}
			return new Polynom(pol, MODULE);
		}
		
		public Polynom sum(Polynom pol) {
			int max = Math.max(pol.size(), size());
			long[] sum = new long[max];
			for (int i = 0; i < max; i++) {
				sum[i] = (this.getOrDefault(i,  0) + pol.getOrDefault(i, 0)) % MODULE;
			}
			return new Polynom(sum, MODULE);
		}
		
		public void set(int pos, long el) {
			if (pos < size()) {
				polynom[pos] = el;
			}
		}
		
		public void filter(long n) {
			long[] a = new long[size()];
			if (n == 0) {
				a = new long[size() / 2 + 1];
			}
			int pos = 0;
			for (int i = 0; i < size(); i++) {
				if (i % 2 == n % 2) {
					a[pos++] = polynom[i];
				}
			}
			polynom = a;
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
