import java.io.*;
import java.util.*;

public class C {
    private final static String GOOD_ANSWER = "YES";
    private final static String BAD_ANSWER = "NO";
    private final static int MAX = 100_000;

    //private static LinkedList<Edge> graph = new LinkedList<>();
    private static long[][] graph;
    private static int size;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        graph = new long[size][size];

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                for (int j = 0; j < size; j++) {
                    int weight = scanner.getNextInt();
                    /*if (weight != MAX *//*&& weight != 0*//*) {
                        graph.add(new Edge(i, j, weight));
                    }*/
                    graph[i][j] = weight == MAX ? Integer.MAX_VALUE : weight;
                }
            }
        }

        if (!negativeSycle()) {
            System.out.println(BAD_ANSWER);
        }
    }

    private static boolean negativeSycle() {
        long[] dp = new long[size];
        int[] parent = new int[size];

        Arrays.fill(dp, Integer.MAX_VALUE);
        Arrays.fill(parent, -1);
        dp[0] = 0;

        int checker = -1;

        for (int i = 0; i < size; i++) {
            /*Iterator<Edge> iterator = graph.iterator();
            while (iterator.hasNext()) {
                Edge edge = iterator.next();
                if (dp[edge.getTo()] > dp[edge.getFrom()] + edge.getWeight()) {
                    dp[edge.getTo()] = dp[edge.getFrom()] + edge.getWeight();
                    parent[edge.getTo()] = edge.getFrom();

                    if (i == size - 1) {
                        checker = edge.getTo();
                    }
                }
            }*/
            for (int j = 0; j < size; j++) {
                for (int k = 0; k < size; k++) {
                    if (dp[k] > dp[j] + graph[j][k]) {
                        dp[k] = dp[j] + graph[j][k];
                        parent[k] = j;
                        if (i == size - 1) {
                            checker = k;
                        }
                    }
                }
            }
        }

        if (checker == -1) {
            return false;
        }

        for (int i = 0; i < size; i++) {
            checker = parent[checker];
        }

        ArrayList<Integer> cycle = new ArrayList<>();

        int u = checker;

        while (u != checker || cycle.size() < 1) {
            cycle.add(u);
            u = parent[u];
        }

        System.out.println(GOOD_ANSWER);

        System.out.println(cycle.size());

        Collections.reverse(cycle);

        for (Integer integer : cycle) {
            System.out.printf("%d ", integer + 1);
        }
        return true;
    }

    private static class Edge {
        private final int from;
        private final int to;
        private final int weight;

        public Edge(int from, int to, int weight) {
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

        public int getWeight() {
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
