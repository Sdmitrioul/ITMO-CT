import java.io.*;
import java.util.*;

public class D {
    private static int N;
    private static int M;
    private static ArrayList<LinkedList<Edge>> graph;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        int start = 0;
        int k = 0;

        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
            k = scanner.getNextInt();
            start = scanner.getNextInt() - 1;
        }

        graph = new ArrayList<>(N);

        for (int i = 0; i < N; i++) {
            graph.add(new LinkedList<>());
        }

        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                int weight = scanner.getNextInt();
                graph.get(from).add(new Edge(to, weight));
            }
        }

        int[] ans = kShortest(start, k);

        for (int an : ans) {
            int out = an == Integer.MAX_VALUE ? -1 : an;
            System.out.printf("%d\n", out);
        }
    }

    private static int[] kShortest(int start, int k) {
        int[][] dp = new int[k + 1][N];
        for (int[] iter : dp) {
            Arrays.fill(iter, Integer.MAX_VALUE);
        }
        dp[0][start] = 0;
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < N; j++) {
                Iterator<Edge> iterator = graph.get(j).iterator();
                while (iterator.hasNext()) {
                    Edge edge = iterator.next();
                    dp[i + 1][edge.getTo()] = dp[i][j] == Integer.MAX_VALUE ? dp[i + 1][edge.getTo()] :
                            Math.min(dp[i + 1][edge.getTo()], dp[i][j] + edge.getWeight());
                }
            }
        }
        return dp[k];
    }

    private static class Edge {
        private final int to;
        private final int weight;

        public Edge(int to, int weight) {
            this.to = to;
            this.weight = weight;
        }

        public int getTo() {
            return to;
        }

        public int getWeight() {
            return weight;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Edge edge = (Edge) o;
            return to == edge.to &&
                    weight == edge.weight;
        }

        @Override
        public int hashCode() {
            return Objects.hash(to, weight);
        }

        @Override
        public String toString() {
            return to + " " + weight;
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
