import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class D {
	private static final int MAX_VALUE = 100000000, SIZE_M = 1000000, SIZE_N = 60000;
	private static int n = 0, m = 0, height = 0, length = 0, from = 0, to = 0, nextIndex = 0;
	private static char[][] map;
	private static long  maxFlow = 0;
	private static Edge[] edges;
	private static int[] flow;
	private static boolean[] used;
	private final static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	private final static Set<Integer> cutting = new HashSet<>();
	
	public static void main(final String[] args) {
		try (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			process();
			output();
		} catch (IOException e) {
			System.err.println("Reading problem: " + e.getMessage());
		}
	}
	
	private static void mark(int vertex) {
		used[vertex] = true;
		
		for (int to : graph.get(vertex)) {
			Edge edge = edges[to];
			
			if (edge.weight - flow[to] > 0 && !used[edge.to]) {
				mark(edge.to);
			}
		}
	}
	
	private static void newInput(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			height = scanner.getNextInt();
			length = scanner.getNextInt();
		}
		
		n = height * length;
		m = 0;
		used = new boolean[SIZE_N];
		
		for (int i = 0; i < SIZE_N; i++) {
			graph.add(new ArrayList<>());
		}
		
		edges = new Edge[SIZE_M];
		flow = new int[SIZE_M];
		
		int k = 0, l = 0;
		if (scanner.hasLine()) {
			k = scanner.getNextInt();
			l = scanner.getNextInt();
		}
		map = new char[height][length];
		for (int i = 0; i < height; i++) {
			Arrays.fill(map[i], '.');
		}
		
		for (int i = 0; i < k; i++) {
			if (scanner.hasLine()) {
				map[scanner.getNextInt() - 1][scanner.getNextInt() - 1] = '#';
			}
		}
		
		for (int i = 0; i < l; i++) {
			if (scanner.hasLine()) {
				map[scanner.getNextInt() - 1][scanner.getNextInt() - 1] = '-';
			}
		}
		
		if (scanner.hasLine()) {
			map[scanner.getNextInt() - 1][scanner.getNextInt() - 1] = 'A';
		}
		
		if (scanner.hasLine()) {
			map[scanner.getNextInt() - 1][scanner.getNextInt() - 1] = 'B';
		}
		
		resolve();
	}
	
	private static int dfs(int vertex, int minCost) {
		if (vertex == to + n) {
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
			height = scanner.getNextInt();
			length = scanner.getNextInt();
		}
		
		n = height * length;
		m = 0;
		used = new boolean[SIZE_N];
		
		for (int i = 0; i < SIZE_N; i++) {
			graph.add(new ArrayList<>());
		}
		
		edges = new Edge[SIZE_M];
		flow = new int[SIZE_M];
		
		String[] input = new String[height];
		
		for (int i = 0; i < height; i++) {
			if (scanner.hasLine()) {
				input[i] = scanner.getLine();
			}
		}
		
		map = parse(input);
		resolve();
	}
	
	private static char[][] parse(final String[] input) {
		char[][] matrix = new char[height][length];
		
		for (int i = 0; i < height; i++) {
			String string = input[i];
			for (int j = 0; j < length; j++) {
				char element = string.charAt(j);
				checkInOut(element, getPosition(i, j));
				matrix[i][j] = element;
			}
		}
		
		return matrix;
	}
	
	private static void resolve() {
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < length; j++) {
				char element = map[i][j];
				int curPos = getPosition(i, j);
				addMirror(curPos, element);
				if (element != '#') {
					Cords up = getCords(i - 1, j);
					Cords left = getCords(i, j - 1);
					Cords down = getCords(i + 1, j);
					Cords right = getCords(i, j + 1);
					checkCords(down, curPos);
					checkCords(right, curPos);
					checkCords(left, curPos);
					checkCords(up, curPos);
				}
			}
		}
		
		for (int i = 0; i < nextIndex; i++) {
			edges[i + nextIndex] = edges[i].getReversed();
			graph.get(edges[i + nextIndex].from).add(i + nextIndex);
		}
		
		m = nextIndex;
	}
	
	private static void addMirror(final int curPos, final char element) {
		int weight = MAX_VALUE;
		
		if (element == '.') {
			weight = 1;
		}
		
		edges[nextIndex] = new Edge(curPos, curPos + n, weight);
		graph.get(curPos).add(nextIndex);
		nextIndex++;
	}
	
	private static void checkCords(final Cords cord, final int from) {
		if (goodCords(cord)) {
			char element = map[cord.row][cord.col];
			if (element != '#') {
				edges[nextIndex] = new Edge(from + n, getPosition(cord), MAX_VALUE);
				graph.get(from + n).add(nextIndex);
				nextIndex++;
			}
		}
	}
	
	private static void checkInOut(final char element, final int position) {
		switch (element) {
			case 'A':
				from = position;
				break;
			case 'B':
				to = position;
				break;
			default:
				//Nothing
		}
	}
	
	private static int getPosition(final int row, final int col) {
		return row * length + col;
	}
	
	private static int getPosition(final Cords cords) {
		return getPosition(cords.row, cords.col);
	}
	
	private static Cords getCords(final int row, final int col) {
		return row >= 0 && col >= 0 && col < length && row < height ? new Cords(row, col) : null;
	}
	
	private static boolean goodCords(final Cords cords) {
		if (cords == null) {
			return false;
		}
		return cords.row < height && cords.row >= 0 && cords.col >= 0 && cords.col < length;
	}
	
	private static void process() {
		while (dfs(from, Integer.MAX_VALUE) != 0) {
			Arrays.fill(used, false);
		}
		
		for (int i : graph.get(from)) {
			maxFlow += flow[i];
		}
		
		if (maxFlow >= MAX_VALUE) {
			return;
		}
		
		Arrays.fill(used, false);
		mark(from);
		
		for (int i = 0; i < 2 * n; i++) {
			if (used[i]) {
				for (int to : graph.get(i)) {
					Edge edge = edges[to];
					
					if (!used[edge.to] && to < m) {
						cutting.add(edges[to % m].from % n);
					}
				}
			}
		}
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			if (maxFlow >= MAX_VALUE) {
				writer.write(-1 + "\n");
				return;
			}
			
			writer.write(cutting.size() + "\n");
			writer.write(getAnswer());
		} catch (IOException e) {
			System.err.println("Problems with output writing");
		}
	}
	
	private static String getAnswer() {
		StringBuilder answer = new StringBuilder();
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < length; j++) {
				char element = cutting.contains(getPosition(i, j)) ? '+' : map[i][j];
				answer.append(element);
			}
			answer.append("\n");
		}
		return answer.toString();
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
	
	private static class Cords {
		final int row;
		final int col;
		
		public Cords(int row, int col) {
			this.row = row;
			this.col = col;
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
