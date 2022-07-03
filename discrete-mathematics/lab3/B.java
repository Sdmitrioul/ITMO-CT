import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class B {
	private static final int MODULE = 998_244_353;
	private static int n = 0, m = 0;
	private static long[] p, sqrt, exp, ln;
	
	public static void main(String[] args) {
		try  (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output(sqrt, exp, ln);
		} catch (IOException e) {
			System.err.println("Reading problem: " + e.getMessage());
		}
	}
	
	private static void process() {
		long[] pol = new long[]{1};
		long factorial = 1;
		
		for (int i = 1; i < m; i++) {
			pol = mul(pol, p);
			
			factorial *= i;
			factorial %= MODULE;
			
			long sqrtK = getSqrtKoef(i);
			long factK = reversedByModule(factorial);
			long lnK = getLnKoef(i);
			
			for (int j = 0; j < Math.min(pol.length, m); j++) {
				sqrt[j] += sqrtK * pol[j] % MODULE;
				exp[j] +=  factK * pol[j] % MODULE;
				ln[j] += lnK * pol[j] % MODULE;
				sqrt[j] %= MODULE;
				exp[j] %= MODULE;
				ln[j] %= MODULE;
			}
		}
	}
	
	private static long mod(long i) {
		return ((i % MODULE) + MODULE) % MODULE;
	}
	
	private static long getLnKoef(int i) {
		int j = i % 2 == 0 ? MODULE - 1 : 1;
		return j * mod(reversedByModule(i)) % MODULE;
	}
	
	private static long getSqrtKoef(int i) {
		long up = 1;
		long down = 1;
		for (int j = 0; j < i; j++) {
			up *= mod(1 - 2 * j);
			down *= (2 * j + 2) % MODULE;
			up %= MODULE;
			down %= MODULE;
		}
		return (up * reversedByModule(down)) % MODULE;
	}
	
	private static long reversedByModule(long n) {
		Pair pair = new Pair();
		if (gcd(n, MODULE, pair) == 1) {
			return mod(pair.a);
		}
		return 0;
	}
	
	private static long gcd(long a, long b, Pair pair) {
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
	
	private static long[] mul(long[] a, long[] b) {
		long[] ans = new long[Math.min(m, a.length + b.length + 1)];
		for (int i = 0; i < a.length; i++) {
			long el = a[i];
			for (int j = 0; j < b.length; j++) {
				if (i + j < m) {
					ans[i + j] += el * b[j];
					ans[i + j] %= MODULE;
				}
			}
		}
		return ans;
	}
	
	private static void output(long[] ... pol) {
		try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			for (long[] arg : pol) {
				write(writer, arg);
				writer.newLine();
			}
		} catch (IOException e) {
			System.err.println("Problems with writing: " + e.getMessage());
		}
	}
	
	private static void write(BufferedWriter writer ,long[] pol) throws IOException {
		for (long el : pol) {
			writer.write(el + " ");
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
			m = scanner.getNextInt();
		}
		
		p = new long[n + 1];
		sqrt = new long[m];
		exp = new long[m];
		ln = new long[m];
		
		sqrt[0] = exp[0] = 1;
		ln[0] = 0;
		
		if (scanner.hasLine()) {
			for (int i = 0; i <= n; i++) {
				p[i] = scanner.getNextInt();
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
	
	private static class Pair {
		long a = Long.MIN_VALUE;
		long b = Long.MIN_VALUE;;
	}
}
