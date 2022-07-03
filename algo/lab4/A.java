import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class A {
    private static int N;
    private static int M;
    private static boolean[] used;
    private static int[] dist;
    private static ArrayList<ArrayList<Integer>> graph;

    public static void main(String[] args) throws IOException {
        graph = new ArrayList<>();
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }

        used = new boolean[N];
        dist = new int[M];

        for (int i = 0; i < N; i++) {
            graph.add(new ArrayList<>());
        }

        Arrays.fill(used, false);
        Arrays.fill(dist, -1);

        for (int i = 0; i < N; i++) {
            if (scanner.hasLine()) {
                int vertex = scanner.getNextInt();
                while (vertex != 0) {
                    graph.get(i).add(vertex - 1);
                    vertex = scanner.getNextInt();
                }
            }
        }


        for (int i  = 0; i < N; i++) {
            Arrays.fill(used, false);
            dfs(i);
        }

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        int counter = 0;
        StringBuilder builder = new StringBuilder();

        for (int i = 0; i < M; i++) {
            if (dist[i] != -1) {
                counter++;
                builder.append(dist[i] + 1).append(" ").append(i + 1).append("\n");
            }
        }

        writer.write(counter + "\n");
        writer.write(builder.toString());

        writer.close();
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

    private static class Scanner {
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
