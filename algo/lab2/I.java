import java.io.*;
import java.util.*;

public class I {
    private static final String DRAW = "DRAW";
    private static final String FIRST = "FIRST";
    private static final String SECOND = "SECOND";

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        while (scanner.hasLine()) {
            int n = scanner.getNextInt();
            int m = scanner.getNextInt();
            int[] out = new int[n];
            ArrayList<LinkedList<Integer>> graph = new ArrayList<>(n);
            for (int i = 0; i < n; i++) {
                graph.add(new LinkedList<>());
            }
            for (int i = 0; i < m; i++) {
                if (scanner.hasLine()) {
                    int from = scanner.getNextInt() - 1;
                    int to = scanner.getNextInt() - 1;
                    out[from]++;
                    graph.get(to).add(from);
                }
            }
            graphAnalysis(graph, out);
        }
    }

    private static void graphAnalysis(ArrayList<LinkedList<Integer>> graph, int[] out) {
        int size = graph.size();
        int[] ans = new int[size];
        int[] outLocal = new int[size];

        PriorityQueue<Integer> queue = new PriorityQueue<>();

        for (int i = 0; i < size; i++) {
            if (out[i] == 0) {
                ans[i] = -1;
                queue.add(i);
            }
        }

        while (!queue.isEmpty()) {
            int vertex = queue.remove();
            Iterator<Integer> iterator = graph.get(vertex).iterator();
            if (ans[vertex] == -1) {
                while (iterator.hasNext()) {
                    int u = iterator.next();
                    if (ans[u] == 0) {
                        ans[u] = 1;
                        queue.add(u);
                    }
                }
            } else {
                while (iterator.hasNext()) {
                    int u = iterator.next();
                    outLocal[u]++;
                    if (outLocal[u] == out[u]) {
                        ans[u] = -1;
                        queue.add(u);
                    }
                }
            }
        }
        for (int i : ans) {
            if (i == 0) {
                System.out.println(DRAW);
            } else if (i == -1) {
                System.out.println(SECOND);
            } else {
                System.out.println(FIRST);
            }
        }
        System.out.println();
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
