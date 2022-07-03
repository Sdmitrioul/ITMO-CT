import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.NoSuchElementException;

public class E {
    private static int size;
    private static ArrayList<ArrayList<Integer>> graph;
    private static int[] parents;
    private static int[] degree;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        graph = new ArrayList<>(size);
        parents = new int[size];
        degree = new int[size];

        for (int i = 0; i < size; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < size - 1; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                graph.get(from).add(to);
                graph.get(to).add(from);
            }
        }

        parents[size - 1] = -1;
        dfs(size - 1);

        int leaf = setDegree();

        ArrayList<Integer> answer = new ArrayList<>();

        int tmpLeaf = leaf;

        for (int i = 0; i + 2 < size; i++) {
            int parent = parents[leaf];
            answer.add(parent);
            degree[parent]--;
            if (degree[parent] == 1 && parent < tmpLeaf) {
                leaf = parent;
            } else {
                tmpLeaf++;
                while (tmpLeaf < size && degree[tmpLeaf] != 1) {
                    tmpLeaf++;
                }
                leaf = tmpLeaf;
            }

        }

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        for (Integer ans : answer) {
            writer.write((ans + 1) + " ");
        }

        writer.flush();
    }

    private static int setDegree() {
        int leaf = -1;
        for (int i = 0; i < size; i++) {
            degree[i] = graph.get(i).size();
            if (degree[i] ==  1  && leaf == -1) {
                leaf = i;
            }
        }
        return leaf;
    }

    private static void dfs(int vertex) {
        for (Integer toVertex : graph.get(vertex)) {
            if (parents[vertex] != toVertex) {
                parents[toVertex] = vertex;
                dfs(toVertex);
            }
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
