import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;

public class E {
    private static final List<Pair<Integer, Integer>> VECTORS = List.of(new Pair<>(-1, 0), new Pair<>(0, -1), new Pair<>(1, 0), new Pair<>(0, 1));
    private static int n,m;
    private static boolean[] used;
    private static int[] dist;
    private static ArrayList<ArrayList<Integer>> graph;

    public static void main(String[] args) {
        int a, b, answer, empty;
        empty = answer = n = m = a = b = 0;
        boolean[] matrix = new boolean[1];
        try (final Scanner scanner = new Scanner(System.in)) {
            if (scanner.hasLine()) {
                n = scanner.getNextInt();
                m = scanner.getNextInt();
                a = scanner.getNextInt();
                b = scanner.getNextInt();
            }

            matrix = new boolean[n * m];

            for (int i = 0; i < n; i++) {
                if (scanner.hasLine()) {
                    String input = scanner.getLine();
                    int j = 0;
                    for (char c : input.toCharArray()) {
                        matrix[i * m + j++] = c == '*';
                        empty = c == '*' ? empty + 1 : empty;
                    }
                }
            }
        } catch (IOException e) {
            System.out.println("Reading goes wrong");
        }

        if (2 * b <= a) {
            answer = empty * b;
        } else {
            buildGraph(matrix);
            int matchingCounter = process();
            answer = matchingCounter * a + (empty - 2 * matchingCounter) * b;
        }

        try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
            writer.write(Integer.toString(answer));
        } catch (IOException ex) {
            System.err.println("Writing goes wrong");
        }
    }

    private static int process() {
        int matchingCounter = 0;
        int summary = n * m;

        for (int i  = 0; i < summary; i++) {
            if (((i % m) % 2 + (i / m) % 2) % 2 == 1) {
                Arrays.fill(used, false);
                dfs(i);
            }
        }

        for (int i = 0; i < summary; i++) {
            if (((i % m) % 2 + (i / m) % 2) % 2 == 0) {
                if (dist[i] != -1) {
                    matchingCounter++;
                }
            }
        }

        return matchingCounter;
    }

    private static void buildGraph(boolean[] matrix) {
        int summary = n * m;
        graph = new ArrayList<>();
        used = new boolean[summary];
        dist = new int[summary];

        for (int i = 0; i < summary; i++) {
            graph.add(new ArrayList<>());
        }

        Arrays.fill(dist, -1);

        for (int i = 0; i < summary; i++) {
            if (((i % m) % 2 + (i / m) % 2) % 2 == 1 && matrix[i]) {
                for (Pair<Integer, Integer> pair : VECTORS) {
                    int row = i / m + pair.getFirst();
                    int column = (i % m) + pair.getSecond();
                    if (goodCoordinates(row, column) && matrix[row * m + column]) {
                        graph.get(i).add(row * m + column);
                    }
                }
            }
        }

    }

    private static boolean goodCoordinates(int row, int column) {
        return row >= 0 && column >= 0 && row < n && column < m;
    }


    private static boolean dfs(int vertex) {
        if (used[vertex]) {
            return false;
        }

        used[vertex] = true;

        for (Integer to : graph.get(vertex)) {
            if (dist[to] == -1 || dfs(dist[to])) {
                dist[to] = vertex;
                return true;
            }
        }

        return false;
    }


    private static class Pair<F, S> {
        private F first;
        private S second;

        public Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }

        public F getFirst() {
            return first;
        }

        public void setFirst(F first) {
            this.first = first;
        }

        public S getSecond() {
            return second;
        }

        public void setSecond(S second) {
            this.second = second;
        }
    }

    private static class Scanner implements Closeable {
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
