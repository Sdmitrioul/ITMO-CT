import java.io.*;
import java.util.*;

public class C {
    private static int N;
    private static int M;
    private static ArrayList<ArrayList<Integer>> arrayLists;
    private static HashSet<Integer> ans = new HashSet<>();
    private static boolean[] mark;
    private static int[] in;
    private static int[] out;
    private static int time;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        time = -1;
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }

        arrayLists = new ArrayList<>();

        for (int i = 0; i < N; i++) {
            arrayLists.add(new ArrayList<>());
        }
        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                arrayLists.get(from).add(to);
                arrayLists.get(to).add(from);
            }
        }
        in = new int[N];
        out = new int[N];
        mark = new boolean[N];
        Arrays.fill(mark, false);
        for (int i = 0; i < N; i++) {
            if (!mark[i]) {
                dfs(i, -1);
            }
        }

        System.out.println(ans.size());

        for (int i = 0; i < N; i++) {
            if (ans.contains(i)) {
                System.out.printf("%d ", i + 1);
            }
        }
    }

    private static void dfs(int root, int parent) {
        time++;
        in[root] = time;
        out[root] = time;
        mark[root] = true;
        int count = 0;
        for (int i = 0; i < arrayLists.get(root).size(); i++) {
            int to = arrayLists.get(root).get(i);
            if (parent == to) continue;
            if (!mark[to]) {
                dfs(to, root);
                count++;
                out[root] = Math.min(out[root], out[to]);
                if (out[to] >= in[root] && parent != -1) {
                    ans.add(root);
                }
            } else {
                out[root] = Math.min(out[root], in[to]);
            }
        }
        if (count > 1 && parent == -1) {
            ans.add(root);
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
