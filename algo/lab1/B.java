import java.io.*;
import java.util.*;

public class B {
    private static int N;
    private static int M;
    private static ArrayList<ArrayList<Integer>> arrayLists;
    private static HashSet<Integer> ans = new HashSet<>();
    private static HashMap<Pair, Integer> ribs;
    private static boolean[] mark;
    private static int[] in;
    private static int[] out;
    private static int time = -1;

    public static void main(String[] args) throws IOException {
        ribs = new HashMap<>();
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }
        arrayLists = new ArrayList<>();
        in = new int[N];
        out = new int[N];
        mark = new boolean[N];
        Arrays.fill(mark, false);
        for (int i = 0; i < N; i++) {
            arrayLists.add(new ArrayList<>());
        }
        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                arrayLists.get(from).add(to);
                arrayLists.get(to).add(from);
                ribs.put(new Pair(from, to), i + 1);
            }
        }

        for (int i = 0; i < N; i++) {
            if (!mark[i]) {
                dfs(i, -1);
            }
        }

        System.out.println(ans.size());

        for (int i = 1; i <= M; i++) {
            if (ans.contains(i)) {
                System.out.printf("%d ", i);
            }
        }
    }

    private static void dfs(int root, int par) {
        time++;
        in[root] = time;
        out[root] = time;
        mark[root] = true;
        for (int i = 0; i < arrayLists.get(root).size(); i++) {
            int to = arrayLists.get(root).get(i);
            if (ribs.get(new Pair(root, to)) == par) continue;
            if (!mark[to]) {
                dfs(to, ribs.get(new Pair(root, to)));
                out[root] = Math.min(out[root], out[to]);
            } else {
                out[root] = Math.min(out[root], in[to]);
            }
        }
        if (out[root] >= in[root] && par != -1) {
            ans.add(par);
        }
    }

    private static class Pair {
        int first;
        int second;

        public Pair(int first, int second) {
            this.first = Math.min(first, second);
            this.second = Math.max(first, second);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Pair pair = (Pair) o;
            return first == pair.first &&
                    second == pair.second;
        }

        @Override
        public int hashCode() {
            return Objects.hash(first, second);
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
