import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class J {
	private static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	private static ArrayList<ArrayList<Pair>> gr = new ArrayList<>();
	private static List<Edge> edgeList = new ArrayList<>();
	private static int n = 0, m = 0, k = 0, nextIndex = 0, from = 0, to = 0;
	private static long answer = 0;
	private static int[] cities;
	private static long[][] roads;
	
	public static void main(String[] args) {
		try (Scanner scanner = new Scanner(System.in)) {
			input(scanner);
			preprocess();
			process();
			output();
		} catch (IOException ex) {
			System.err.println("Reading goes wrong");
		}
	}
	
	private static void input(final Scanner scanner) throws IOException {
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
			m = scanner.getNextInt();
		}
		
		cities = new int[n];
		
		if (scanner.hasLine()) {
			for (int i = 0; i < n; i++) {
				cities[i] = scanner.getNextInt();
			}
		}
		
		roads = new long[n][n];
		
		for (int i = 0; i < n; i++) {
			Arrays.fill(roads[i], Long.MAX_VALUE);
			roads[i][i] = 0;
			gr.add(new ArrayList<>());
		}
		
		for (int i = 0; i < m; i++) {
			if (scanner.hasLine()) {
				int from = scanner.getNextInt() - 1;
				int to = scanner.getNextInt() - 1;
				int cost = scanner.getNextInt();
				roads[from][to] = cost;
				gr.get(from).add(new Pair(to, cost));
			}
		}
	}
	
	private static void preprocess() {
		for (int i = 0; i < 2 * n + 2; i++) {
			graph.add(new ArrayList<>());
		}
		
		for (int i = 0; i < n; i++) {
			long[] d = new long[n];
			int[] p = new int[n];
			Arrays.fill(d, Long.MAX_VALUE);
			d[i] = 0;
			boolean[] used = new boolean[n];
			for (int j = 0; j < n; j++) {
				int v = -1;
				for (int l = 0; l < n; l++) {
					if (!used[l] && (v == -1 || d[l] < d[v])) {
						v = l;
					}
				}
				if (d[v] == Long.MAX_VALUE) {
					break;
				}
				used[v] = true;
				for (Pair pair : gr.get(v)) {
					if (d[v] + pair.weight < d[pair.to]) {
						d[pair.to] = d[v] + pair.weight;
						p[pair.to] = v;
					}
				}
			}
			
			for (int j = 0; j < n; j++) {
				if (d[j] != Long.MAX_VALUE && i != j) {
					addEdge(i, j + n, 1, d[j]);
				}
			}
		}
		
		from = 2 * n;
		to = 2 * n + 1;
		
		for (int i = 0; i < n; i++) {
			addEdge(from, i, 1, 0);
			addEdge(i + n, to, 1, 0);
			addEdge(i, n + i, 1, cities[i]);
		}
		
		m = nextIndex;
	}
	
	private static void process() {
		int[] parents = new int[graph.size()];
		Arrays.fill(parents, -1);
		while(pathExists(parents)) {
			int flow = getFlow(from, to, parents);
			for (int i = to; i != from; i = edgeList.get(parents[i]).from) {
				int pos = parents[i];
				edgeList.get(pos).addToFlow(flow);
				edgeList.get(pos + (pos % 2 == 0 ? 1 : -1)).addToFlow(-flow);
				answer += edgeList.get(pos).cost * flow;
			}
			Arrays.fill(parents, -1);
		}
	}
	
	private static boolean pathExists(final int[] parents) {
		final long[] dist = new long[graph.size()];
		
		Arrays.fill(dist, Long.MAX_VALUE);
		dist[from] = 0;
		final Deque<Integer> queue = new ArrayDeque<>();
		queue.addLast(from);
		
		while (!queue.isEmpty()) {
			int from = queue.pollFirst();
			for (int to : graph.get(from)) {
				Edge edge = edgeList.get(to);
				if (dist[edge.to] <= dist[edge.from] + edge.cost || edge.weight - edge.flow == 0) {
					continue;
				}
				queue.remove(edge.to);
				dist[edge.to] = dist[edge.from] + edge.cost;
				parents[edge.to] = to;
				queue.add(edge.to);
			}
		}
		
		return dist[to] != Long.MAX_VALUE ;
	}
	
	private static int getFlow(final int to, final int from, final int[] parents) {
		if (to == from) {
			return Integer.MAX_VALUE;
		}
		
		return Math.min(edgeList.get(parents[from]).getCurFlow(), getFlow(to, edgeList.get(parents[from]).from, parents));
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(answer + "\n");
		} catch (IOException ex) {
			System.err.println("Writing goes wrong");
		}
	}
	
	private static void addEdge(final int from, final int to, final int weight, final long cost) {
		edgeList.add(new Edge(from, to, weight, cost));
		edgeList.add(edgeList.get(nextIndex).getReversed());
		graph.get(from).add(nextIndex);
		graph.get(to).add(nextIndex + 1);
		nextIndex += 2;
	}
	
	private static class Edge {
		final int from;
		final int to;
		final int weight;
		final long cost;
		private int flow = 0;
		
		public Edge(int from, int to, int weight, long cost) {
			this.from = from;
			this.to = to;
			this.weight = weight;
			this.cost = cost;
		}
		
		public int getFlow() {
			return flow;
		}
		
		public int getCurFlow() {
			return weight - flow;
		}
		
		public void setFlow(int flow) {
			this.flow = flow;
		}
		
		public void addToFlow(int addingFlow) {
			this.flow += addingFlow;
		}
		
		public long getCostOfFlow() {
			return flow * cost;
		}
		
		public Edge getReversed() {
			return new Edge(to, from, 0, -cost);
		}
	}
	
	private static class Pair {
		final int to;
		final int weight;
		
		public Pair(int to, int weight) {
			this.to = to;
			this.weight = weight;
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
