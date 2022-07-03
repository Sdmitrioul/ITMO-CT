import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class E {
    private static int N;
    private static int M;
    private static ArrayList<ArrayList<Pair>> arrayLists;
    private static boolean[] mark;
    private static int[] in;
    private static int[] out;
    private static int[] components;
    private static int time = -1;
    private static int colours = 0;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }

        arrayLists = new ArrayList<>();
        in = new int[N];
        out = new int[N];
        components = new int[M];
        mark = new boolean[N];

        Arrays.fill(components, 0);
        Arrays.fill(mark, false);

        for (int i = 0; i < N; i++) {
            arrayLists.add(new ArrayList<>());
        }
        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                arrayLists.get(from).add(new Pair(to, i));
                arrayLists.get(to).add(new Pair(from, i));
            }
        }

        for (int i = 0; i < N; i++) {
            if (!mark[i]) {
                dfs(i, -1);
            }
        }

        Arrays.fill(mark, false);

        for (int i = 0; i < N; i++) {
            if (!mark[i] && arrayLists.get(i).size() != 0) {
                paint(i, -1, colours);
            }
        }

        //System.out.println(colours);

        int max = 0;
        for (int i = 0; i < M; i++) {
            max = Math.max(max, components[i]);
        }

        System.out.println(max);

        for (int i = 0; i < M; i++) {
            System.out.printf("%d ", components[i]);
        }
    }

    private static void paint(int root, int parent, int color) {
        mark[root] = true;
        for (int i = 0; i < arrayLists.get(root).size(); i++) {
            int to = arrayLists.get(root).get(i).first;
            int number = arrayLists.get(root).get(i).n;
            if (to == parent) {
                components[number] = components[number] == 0 ? color : components[number];
                continue;
            }
            if (!mark[to]) {
                if (out[to] >= in[root]) {
                    components[number] = ++colours;
                    paint(to, root, components[number]);
                } else {
                    components[number] = color;
                    paint(to, root, color);
                }
            } else if (components[number] == 0) {
                components[number] = color;
            }
        }
    }

    private static void dfs(int root, int parent) {
        time++;
        in[root] = time;
        out[root] = time;
        mark[root] = true;
        for (int i = 0; i < arrayLists.get(root).size(); i++) {
            int to = arrayLists.get(root).get(i).first;
            if (parent == to) continue;
            if (!mark[to]) {
                dfs(to, root);
                out[root] = Math.min(out[root], out[to]);
            } else {
                out[root] = Math.min(out[root], in[to]);
            }
        }
    }

    private static class Pair {
        int first;
        int n;

        public Pair(int first, int n) {
            this.first = first;
            this.n = n;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Pair pair = (Pair) o;
            return first == pair.first &&
                    n == pair.n;
        }

        @Override
        public int hashCode() {
            return Objects.hash(first, n);
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
