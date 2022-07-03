import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class F {
	private static int size = 0, from, to, m, nextPos = 0;
	private static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	private static Edge[] edges;
	private static int[] inputScore, outputScore, flow;
	private static boolean[] used;
	private static char[][] map;
	
	public static void main(String[] args) {
		try (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			preprocess();
			process();
			output();
		} catch (IOException e) {
			System.err.println("Reading goes wrong");
		}
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(getAnswer() );
		} catch (IOException e) {
			System.err.println("Output writing goes wrong");
		}
	}
	
	private static String getAnswer() {
		final String newLine = "\n";
		final StringBuilder answer = new StringBuilder();
		
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				answer.append(map[i][j]);
			}
			answer.append(newLine);
		}
		
		return answer.toString();
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
	
	private static void process() {
		while (dfs(from, Integer.MAX_VALUE) != 0) {
			Arrays.fill(used, false);
		}
		
		for (int i = 0; i < size; i++) {
			for (int edgeNum : graph.get(i)) {
				Edge edge = edges[edgeNum];
				if (edge.to == to || edge.to == from || edge.to > edge.from || map[edge.from][edge.to] != '.') {
					continue;
				}
				switch (Math.abs(flow[edgeNum])) {
					case 3:
						map[edge.from][edge.to] = 'W';
						map[edge.to][edge.from] = 'L';
						break;
					case 2:
						map[edge.from][edge.to] = 'w';
						map[edge.to][edge.from] = 'l';
						break;
					case 1:
						map[edge.from][edge.to] = 'l';
						map[edge.to][edge.from] = 'w';
						break;
					case 0:
						map[edge.from][edge.to] = 'L';
						map[edge.to][edge.from] = 'W';
						break;
					default:
						//Ignore;
						break;
				}
			}
		}
	}
	
	private static void preprocess() {
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				char element = map[i][j];
				switch (element) {
					case 'W':
						outputScore[i] -= 3;
						break;
					case 'w':
						outputScore[i] -= 2;
						break;
					case 'l':
						outputScore[i] -= 1;
						break;
					case '.':
						if (i < j) {
							inputScore[i] += 3;
							addEdge(i, j, 3);
						}
						break;
					default:
						//Ignore;
						break;
				}
			}
			addEdge(from, i, inputScore[i]);
			addEdge(i, to, outputScore[i]);
		}
	}
	
	private static void addEdge(final int from, final int to, final int weight) {
		edges[nextPos] = new Edge(from, to, weight);
		graph.get(from).add(nextPos);
		edges[nextPos + m] = edges[nextPos].getReversed();
		graph.get(to).add(nextPos + m);
		nextPos++;
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			size = scanner.getNextInt();
		}
		
		for (int i = 0; i < size + 2; i++) {
			graph.add(new ArrayList<>());
		}
		
		from = size;
		to = size + 1;
		m = size * 2;
		
		map = new char[size][size];
		inputScore = new int[size];
		outputScore = new int[size];
		used = new boolean[size + 2];
		
		for (int i = 0; i < size; i++) {
			if (scanner.hasLine()) {
				String input = scanner.getLine();
				for (int j = 0; j < size; j++) {
					char element = input.charAt(j);
					map[i][j] = element;
					if (element == '.' && i < j) {
						m++;
					}
				}
			}
		}
		
		flow = new int[2 * m];
		edges = new Edge[2 * m];
		
		if (scanner.hasLine()) {
			for (int i = 0; i < size; i++) {
				outputScore[i] = scanner.getNextInt();
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
