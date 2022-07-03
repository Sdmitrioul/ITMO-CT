import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

public class G {
	private static String input = "";
	private static long[] ans;
	
	public static void main(String[] args) {
		try  (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output(ans);
		} catch (IOException e) {
			System.err.println("Reading problem: " + e.getMessage());
		}
	}
	
	private static void process() {
		CombParser parser = new CombParser();
		ans = parser.parse(input);
	}
	
	private static void output(long[] ... pol) {
		try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			for (long[] arg : pol) {
				writer.write(Arrays.stream(arg).mapToObj(Long::toString).collect(Collectors.joining(" ")));
				writer.newLine();
			}
		} catch (IOException e) {
			System.err.println("Problems with writing: " + e.getMessage());
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			input = scanner.getLine();
		}
	}
	
	private static class CombParser {
		private String string;
		private int pos = -1;
		
		public long[] parse(String input) {
			this.string = input;
			long[] result = parseExpr();
			clear();
			return result;
		}
		
		private long[] parseExpr() {
			long[] ans = new long[]{0, 1, 0, 0, 0, 0, 0};
			
			char element = next();
			
			switch (element) {
				case 'B':
					break;
				
				case 'L':
					checkNext('(');
					ans = makeL(parseExpr());
					checkNext(')');
					break;
				
				case 'S':
					checkNext('(');
					ans = makeS(parseExpr());
					checkNext(')');
					break;
				
				case 'P':
					checkNext('(');
					long[] left = parseExpr();
					checkNext(',');
					long[] right = parseExpr();
					checkNext(')');
					ans = mul(left, right, 7);
					break;
				
				default:
					throw new RuntimeException("Wrong argument at position - " + pos + "; element - " + element);
			}
			
			return ans;
		}
		
		private long[] makeL(long[] expr) {
			long[] ans = new long[7];
			ans[0] = 1;
			
			for (int i = 1; i < 7; i++) {
				for (int j = 1; j <= i; j++) {
					ans[i] += expr[j] * ans[i - j];
				}
			}
			
			return ans;
		}
		
		private long[] makeS(long[] expr) {
			long[] ans = new long[7];
			long[][] multi = new long[7][7];
			ans[0] = 1;
			
			for (int i = 0; i < 7; i++) {
				multi[i][0] = 0;
				multi[0][i] = 1;
			}
			
			for (int i = 1; i < 7; i++) {
				for (int j = 1; j < 7; j++) {
					for (int k = 0; k <= i / j; k++) {
						multi[i][j] += binomic(expr[j] + k - 1, k) * multi[i - k * j][j - 1];
					}
				}
			}
			
			for (int i = 1; i < 7; i++) {
				ans[i] = multi[i][i];
			}
			
			return ans;
		}
		
		private boolean hasNext() {
			return pos < string.length() - 1;
		}
		
		private char next() {
			if (!hasNext()) {
				throw new RuntimeException("Out of bound exception");
			}
			return string.charAt(++pos);
		}
		
		private void checkNext(char a) {
			char next = next();
			if (next != a) {
				throw new RuntimeException("Wrong input at position - " + pos + "; expected - " + a + "; got - " + next);
			}
		}
		
		public void clear() {
			pos = -1;
			string = null;
		}
		
		private long binomic(long n, long k) {
			n = Math.max(0, n);
			long answer = 1;
			
			for (long i = n - k + 1; i <= n; i++) {
				answer *= i;
			}
			
			for (long i = 1; i <= k; i++) {
				answer /= i;
			}
			
			return answer;
		}
		
		private long[] mul(long[] a, long[] b, int m) {
			long[] ans = new long[Math.min(m, a.length + b.length + 1)];
			for (int i = 0; i < 7; i++) {
				for (int j = 0; j <= i; j++) {
					ans[i] += a[j] * b[i - j];
				}
			}
			return ans;
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
