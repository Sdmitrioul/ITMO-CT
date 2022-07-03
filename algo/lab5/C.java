import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

public class C {
	private static int n = 0, m = 0, maxFlow = 0, from = 0, to = 0;
	private static Edge[] edges;
	private static int[] flow;
	private static boolean[] used;
	private final static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	
	public static void main(final String[] args) {
		try  (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output();
		} catch (IOException e) {
			System.err.println("Reading problem: " + e.getMessage());
		}
	}
	
	private static int dfs(int vertex, int minCost) {
		if (vertex == to) {
			return minCost;
		}
		
		used[vertex] = true;
		
		for (int to : graph.get(vertex)) {
			Edge edge = edges[to];
			
			if (!used[edge.to] && flow[to] < edge.weight) {
				int delta = dfs(edge.to, Math.min(minCost, edge.weight - flow[to]));
				
				if (delta > 0) {
					flow[to] += delta;
					flow[(to + m) % (2 * m)] -= delta;
					return delta;
				}
			}
		}
		
		return 0;
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
			m = scanner.getNextInt();
			from = scanner.getNextInt() - 1;
			to = scanner.getNextInt() - 1;
		}
		
		used = new boolean[n];
		
		for (int i = 0; i < n; i++) {
			graph.add(new ArrayList<>());
		}
		
		edges = new Edge[2 * m];
		flow = new int[2 * m];
		
		for (int i = 0; i < m; i++) {
			if (scanner.hasLine()) {
				Edge edge = new Edge(scanner.getNextInt() - 1, scanner.getNextInt() - 1, 1);
				edges[i] = edge;
				edges[i + m] = edge.getReversed();
				graph.get(edge.from).add(i);
				graph.get(edge.to).add(i + m);
			}
		}
	}
	
	private static void process() {
		while (dfs(from, Integer.MAX_VALUE) != 0) {
			Arrays.fill(used, false);
		}
		
		for (int i : graph.get(from)) {
			if (i < m) {
				maxFlow += flow[i];
			}
		}
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(getAnswer());
		} catch (IOException e) {
			System.err.println("Problems with output writing");
		}
	}
	
	private static String getAnswer() {
		if (maxFlow < 2) {
			return "NO";
		}
		
		StringBuilder sb = new StringBuilder("YES\n");
		sb.append(findPas(from).stream().map(x -> Integer.toString(x + 1)).collect(Collectors.joining(" ")));
		sb.append("\n");
		sb.append(findPas(from).stream().map(x -> Integer.toString(x + 1)).collect(Collectors.joining(" ")));
		
		return sb.toString();
	}
	
	private static List<Integer> findPas(final int from) {
		Arrays.fill(used, false);
		List<Integer> list = new ArrayList<>();
		findPas(from, list);
		return list;
	}
	
	private static void findPas(final int from, final List<Integer> list) {
		list.add(from);
		if (from == to) {
			return;
		}
		
		used[from] = true;
		
		for (int to : graph.get(from)) {
			Edge edge = edges[to];
			if (to < m && !used[edge.to] && flow[to] > 0) {
				findPas(edge.to, list);
				flow[to] = 0;
				return;
			}
		}
	}
	
	private static class Edge {
		final int from;
		final int to;
		final int weight;
		
		public Edge(int from, int to, int weight) {
			this.from = from;
			this.to = to;
			this.weight = weight;
		}
		
		public Edge getReversed() {
			return new Edge(to, from, 0);
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
