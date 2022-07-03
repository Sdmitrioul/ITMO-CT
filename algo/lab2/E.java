import java.io.*;
import java.util.*;

public class E {
    private static final String UNREACHABLE = "*";
    private static final String TOOSMALL = "-";

    private static ArrayList<LinkedList<Edge>> graph;
    private static int size;
    private static int edgesSize;

    private static boolean[] mark;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        int starter = 0;

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
            edgesSize = scanner.getNextInt();
            starter = scanner.getNextInt() - 1;
        }

        graph = new ArrayList<>(size);
        mark = new boolean[size];
        for (int i = 0; i < size; i++) {
            graph.add(new LinkedList<>());
        }

        for (int i = 0; i < edgesSize; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                long weight = scanner.getNextLong();
                graph.get(from).add(new Edge(from, to, weight));
            }
        }

        long[] ans = smartFord(starter);

        for (int i = 0; i < size; i++) {
            if (mark[i]) {
                System.out.println(TOOSMALL);
            } else {
                System.out.println(ans[i] == Long.MAX_VALUE ? UNREACHABLE : ans[i]);
            }
        }
    }

    private static long[] smartFord(int start) {
        long[] dp = new long[size];
        Arrays.fill(dp, Long.MAX_VALUE);
        dp[start] = 0;
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                Iterator<Edge> iterator = graph.get(j).iterator();
                while (iterator.hasNext()) {
                    Edge edge = iterator.next();
                    if (dp[edge.getFrom()] != Long.MAX_VALUE && dp[edge.getTo()] > dp[edge.getFrom()] + edge.getWeight()) {
                        dp[edge.getTo()] = dp[edge.getFrom()] + edge.getWeight();
                        if (i == size - 1) {
                            if (!mark[edge.getTo()]) {
                                dfs(edge.getTo());
                            }
                        }
                    }
                }
            }
        }
        return dp;
    }

    private static void dfs(int to) {
        mark[to] = true;
        Iterator<Edge> iterator = graph.get(to).iterator();
        while (iterator.hasNext()) {
            Edge edge = iterator.next();
            if (!mark[edge.getTo()]) {
                dfs(edge.getTo());
            }
        }
    }

    private static class Edge {
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
