import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class K {
    private static final String FIRST = "1";
    private static final String SECOND = "2";

    private static ArrayList<ArrayList<Edge>> graph;
    private static int size;
    private static int root;
    private static int[] grandi;
    private static boolean[] mark;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
            root = scanner.getNextInt() - 1;
        }

        grandi = new int[size];
        Arrays.fill(grandi, -1);
        mark = new boolean[size];
        graph = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < size - 1; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                graph.get(from).add(new Edge(from, to, i + 1));
                graph.get(to).add(new Edge(to, from, i + 1));
            }
        }

        dfs(root);

        if (grandi[root] == 0) {
            System.out.println(SECOND);
        } else {
            Arrays.fill(mark, false);
            System.out.println(FIRST);
            System.out.println(move(root, 0));
        }
    }

    private static void dfs(int vertex) {
        mark[vertex] = true;
        int gver = 0;
        for (Edge edge : graph.get(vertex)) {
            if (!mark[edge.getTo()]) {
                dfs(edge.getTo());
                gver = gver ^ grandi[edge.getTo()] + 1;
            }
        }
        grandi[vertex] = gver;
    }

    private static int move(int vertex, int needVgr) {
        mark[vertex] = true;
        for (Edge edge : graph.get(vertex)) {
            if (!mark[edge.getTo()]) {
                int need = needVgr ^ grandi[vertex] ^ (grandi[edge.getTo()] + 1);
                if (need == 0) {
                    return edge.getNumber();
                } else {
                    int move = move(edge.getTo(), need - 1);
                    if (move != -1) {
                        return move;
                    }
                }
            }
        }
        return -1;
    }

    private static class Edge {
        private final int from;
        private final int to;
        private final int number;

        public Edge(int from, int to, int number) {
            this.from = from;
            this.to = to;
            this.number = number;
        }

        public int getFrom() {
            return from;
        }

        public int getTo() {
            return to;
        }

        public int getNumber() {
            return number;
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
