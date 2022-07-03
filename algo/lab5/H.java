import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

public class H {
	private static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	private static List<String> ans = new ArrayList<>();
	private static int n = 0;
	private static long answer = 0;
	private static long[][] input;
	
	public static void main(String[] args) {
		try (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output();
		} catch (IOException ex) {
			System.err.println("Reading goes wrong");
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
		}
		
		input = new long[n + 1][n + 1];
		
		for (int i = 0; i < n; i++) {
			if (scanner.hasLine()) {
				for (int j = 0; j < n; j++) {
					input[i + 1][j + 1] = scanner.getNextInt();
				}
			}
		}
	}
	
	private static void process() {
		long[] u, v;
		long[] p, way;
		u = new long[n + 1];
		v = new long[n + 1];
		p = new long[n + 1];
		way = new long[n + 1];
		
		for (int i = 1; i <= n ; i++) {
			p[0] = i;
			int j0 = 0;
			long[] minV = new long[n + 1];
			Arrays.fill(minV, Long.MAX_VALUE);
			boolean[] used = new boolean[n + 1];
			do {
				used[j0] = true;
				long i0 = p[j0], j1 = 0;
				long delta = Long.MAX_VALUE;
				for (int j = 1; j <= n; ++j)
					if (!used[j]) {
						long cur = input[(int) i0][j]-u[(int) i0]-v[j];
						if (cur < minV[j]) {
							minV[j] = cur;
							way[j] = j0;
						}
						if (minV[j] < delta) {
							delta = minV[j];
							j1 = j;
						}
					}
				for (int j = 0; j <= n; ++j)
					if (used[j]) {
						u[(int) p[j]] += delta;
						v[j] -= delta;
					} else {
						minV[j] -= delta;
					}
				j0 = (int) j1;
			} while (p[j0] != 0);
			do {
				long j1 = way[j0];
				p[j0] = p[(int) j1];
				j0 = (int) j1;
			} while (j0 != 0);
		}
		answer = -v[0];
		for (int i = 1; i <= n; i++) {
			ans.add(p[i] + " " + i);
		}
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(answer + "\n");
			writer.write(ans.stream().collect(Collectors.joining("\n")));
		} catch (IOException ex) {
			System.err.println("Writing goes wrong");
		}
	}
	
	private static class Scanner implements AutoCloseable {
		private final BufferedReader reader;
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
