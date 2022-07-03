import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class I {
	private static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	private static Edge[] edges;
	private static int n = 0, m = 0, nextIndex = 0;
	private static long answer = 0;
	
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
		int[] firstPlayer = new int[3];
		int[] secondPlayer = new int[3];
		
		n = 8;
		m = 15;
		
		edges = new Edge[2 * m];
		
		for (int i = 0; i < n; i++) {
			graph.add(new ArrayList<>());
		}
		
		if (scanner.hasLine()) {
			for (int i = 0; i < 3; i++) {
				addEdge(0, i + 1, scanner.getNextInt(), 0);
			}
		}
		
		if (scanner.hasLine()) {
			for (int i = 0; i < 3; i++) {
				addEdge(i + 4, 7, scanner.getNextInt(), 0);
			}
		}
		
		addEdge(1, 4, Integer.MAX_VALUE, 0);
		addEdge(1, 5, Integer.MAX_VALUE, 1);
		addEdge(1, 6, Integer.MAX_VALUE, 0);
		
		addEdge(2, 4, Integer.MAX_VALUE, 0);
		addEdge(2, 5, Integer.MAX_VALUE, 0);
		addEdge(2, 6, Integer.MAX_VALUE, 1);
		
		addEdge(3, 4, Integer.MAX_VALUE, 1);
		addEdge(3, 5, Integer.MAX_VALUE, 0);
		addEdge(3, 6, Integer.MAX_VALUE, 0);
	}
	
	private static void addEdge(final int from, final int to, final int weight, final int cost) {
		edges[nextIndex] = new Edge(from, to, weight, cost);
		edges[nextIndex + m] = edges[nextIndex].getReversed();
		graph.get(from).add(nextIndex);
		graph.get(to).add(nextIndex + m);
		nextIndex++;
	}
	
	private static void process() {
		int[] parents = new int[n];
		Arrays.fill(parents, -1);
		while(pathExists(parents)) {
			int flow = getFlow(0, n - 1, parents);
			for (int i = n - 1; i != 0; i = edges[parents[i]].from) {
				int pos = parents[i];
				edges[pos].addToFlow(flow);
				edges[(pos + m) % (2 * m)].addToFlow(-flow);
			}
			Arrays.fill(parents, -1);
		}
		
		for (int i = 0; i < m; i++) {
			answer += edges[i].getCostOfFlow();
		}
	}
	
	private static int getFlow(final int to, final int from, final int[] parents) {
		if (to == from) {
			return Integer.MAX_VALUE;
		}
		
		return Math.min(edges[parents[from]].getCurFlow(), getFlow(to, edges[parents[from]].from, parents));
	}
	
	private static boolean pathExists(final int[] parents) {
		final int[] dist = new int[n];
		
		Arrays.fill(dist, Integer.MAX_VALUE);
		dist[0] = 0; // Если не поставить это, хуй а не 51 тест(((
		final Deque<Integer> queue = new ArrayDeque<>();
		queue.addLast(0);
		
		while (!queue.isEmpty()) {
			int from = queue.pollFirst();
			for (int to : graph.get(from)) {
				Edge edge = edges[to];
				if (dist[edge.to] <= dist[edge.from] + edge.cost || edge.weight - edge.flow == 0) {
					continue;
				}
				queue.remove(edge.to);
				dist[edge.to] = dist[edge.from] + edge.cost;
				parents[edge.to] = to;
				queue.add(edge.to);
			}
		}
		
		return dist[n - 1] != Integer.MAX_VALUE ;
	}
	
	private static void output() {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(answer + "");
		} catch (IOException ex) {
			System.err.println("Writing goes wrong");
		}
	}
	
	private static class Edge {
		final int from;
		final int to;
		final int weight;
		final int cost;
		private int flow = 0;
		
		public Edge(int from, int to, int weight, int cost) {
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
		
		public int getCostOfFlow() {
			return flow * cost;
		}
		
		public Edge getReversed() {
			return new Edge(to, from, 0, -cost);
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
