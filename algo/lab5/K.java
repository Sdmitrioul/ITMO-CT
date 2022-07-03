import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

public class K {
	private static ArrayList<ArrayList<Integer>> graph = new ArrayList<>();
	private static List<String> ans = new ArrayList<>();
	private static Edge[] edges;
	private static int n = 0, m = 0, k = 0, nextIndex = 0, from = 0, to = 0;
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
		if (scanner.hasLine()) {
			n = scanner.getNextInt();
			m = scanner.getNextInt();
			k = scanner.getNextInt();
		}
		
		from = 0;
		to = n - 1;
		
		edges = new Edge[4 * m];
		
		for (int i = 0; i < n; i++) {
			graph.add(new ArrayList<>());
		}
		
		for (int i = 0; i < m; i++) {
			if (scanner.hasLine()) {
				int first = scanner.getNextInt() - 1;
				int second = scanner.getNextInt() - 1;
				int cost = scanner.getNextInt();
				addEdge(first, second, 1, cost);
				addEdge(second, first, 1, cost);
			}
		}
	}
	
	private static void addEdge(final int from, final int to, final int weight, final int cost) {
		edges[nextIndex] = new Edge(from, to, weight, cost);
		edges[nextIndex + 2 * m] = edges[nextIndex].getReversed();
		graph.get(from).add(nextIndex);
		graph.get(to).add(nextIndex + 2 * m);
		nextIndex++;
	}
	
	private static void process() {
		int[] parents = new int[graph.size()];
		Arrays.fill(parents, -1);
		for (int j = 0; j < k; j++) {
			if (!pathExists(parents)) {
				answer = -1;
				return;
			}
			int flow = getFlow(from, to, parents);
			for (int i = to; i != from; i = edges[parents[i]].from) {
				int pos = parents[i];
				edges[pos].addToFlow(flow);
				edges[(pos + 2 * m) % (4 * m)].addToFlow(-flow);
			}
			Arrays.fill(parents, -1);
		}
		
		for (int i = 0; i < 2 * m; i++) {
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
		final int[] dist = new int[graph.size()];
		
		Arrays.fill(dist, Integer.MAX_VALUE);
		dist[from] = 0; // Если не поставить это, хуй а не 51 тест(((
		final Deque<Integer> queue = new ArrayDeque<>();
		queue.addLast(from);
		
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
			if (answer  != -1) {
				writer.write(String.format("%.5f\n", (double) answer / k));
				writer.write(getAnswer());
			} else {
				writer.write("-1\n");
			}
		} catch (IOException ex) {
			System.err.println("Writing goes wrong");
		}
	}
	
	private static String getAnswer() {
		StringBuilder sb = new StringBuilder();
		List<Integer> list = new ArrayList<>();
		for (int i = 0; i < k; i++) {
			findPass(from, to, list);
			sb.append(list.size()).append(" ").append(list.stream().map(x -> Integer.toString(x + 1)).collect(Collectors.joining(" "))).append("\n");
			list = new ArrayList<>();
		}
		
		return sb.toString();
	}
	
	private static void findPass(int from, int to, List<Integer> list) {
		if (from == to) {
			return;
		}
		
		for (int way : graph.get(from)) {
			Edge edge = edges[way];
			if (edge.flow == edge.weight && edge.weight != 0) {
				edges[way].flow = edges[(way + 2 * m) % (4 * m)].flow = 0;
				list.add(way / 2);
				findPass(edge.to, to, list);
				break;
			}
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
