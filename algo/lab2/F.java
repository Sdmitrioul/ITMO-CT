import java.io.*;
import java.util.*;

public class F {
    private static final int BAD = -1;
    private static final long MAX = (long) 1e18;

    private static ArrayList<ArrayList<Edge>> graph;
    private static int size;
    private static int edgesSize;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        int a, b, c;

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
            edgesSize = scanner.getNextInt();
        }

        a = b = c = 0;
        graph = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < edgesSize; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                long weight = scanner.getNextLong();
                graph.get(from).add(new Edge(from, to, weight));
                graph.get(to).add(new Edge(to, from, weight));
            }
        }

        if (scanner.hasLine()) {
            a = scanner.getNextInt() - 1;
            b = scanner.getNextInt() - 1;
            c = scanner.getNextInt() - 1;
        }

        long[] distB = dijkstra(b);
        if (distB[a] == MAX || distB[c] == MAX) {
            System.out.println(BAD);
        } else {
            long[] distA = dijkstra(a);
            if (distA[c] == MAX) {
                System.out.println(BAD);
            } else {
                long abc = distA[b] + distB[c];
                long acb = distA[c] + distB[c];
                long bac = distB[a] + distA[c];
                System.out.println(Math.min(abc, Math.min(acb, bac)));
            }
        }
    }

    private static long[] dijkstra(int s) {
        long[] distance = new long[size];
        Arrays.fill(distance, MAX);
        Queue<Edge> queue = new PriorityQueue<>();
        queue.add(new Edge(s, s, 0));
        distance[s] = 0;
        while (!queue.isEmpty()){
            Edge v = queue.poll();
            if (distance[v.to] < v.weight) {
                continue;
            }
            for (Edge edge : graph.get(v.getTo())) {
                long nleng = distance[v.getTo()] + edge.weight;
                if (nleng < distance[edge.to]) {
                    distance[edge.to] = nleng;
                    queue.add(edge);
                }
            }
        }
        return distance;
    }

    private static class Edge implements Comparable {
        private final int from;
        private final int to;
        private final long weight;

        public Edge(int from, int to, long weight) {
            this.from = from;
            this.to = to;
            this.weight = weight;
        }

        public int getFrom() {
            return from;
        }

        public int getTo() {
            return to;
        }

        public long getWeight() {
            return weight;
        }

        @Override
        public int compareTo(Object o) {
            Edge edge = (Edge) o;
            return Long.compare(this.getWeight(), edge.getWeight());
        }
    }

    private static class Scanner {
        private BufferedReader reader;
        private String line;
        private boolean readed;
        private int index = 0;
        private int position = 0;
        private String mark;

        public Scanner(InputStream in) throws UnsupportedEncodingException {
            try{
                reader = new BufferedReader(new InputStreamReader(in, "utf8"));
            } catch (UnsupportedEncodingException e){
                throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
            }
        }

        public Scanner(String s) throws FileNotFoundException, UnsupportedEncodingException {
            try{
                reader = new BufferedReader(new InputStreamReader(new FileInputStream(s), "utf8"));
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
