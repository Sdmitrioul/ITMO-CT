import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class F {
    private static int N;
    private static int M;
    private static ArrayList<ArrayList<Integer>> arrayLists, arrayListsSecond;
    private static ArrayList<Integer> ord;
    private static HashSet<Pair> set = new HashSet<>();
    private static boolean[] mark;
    private static int[] components;
    private static int colour = 1;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }

        arrayLists = new ArrayList<>();
        ord = new ArrayList<>();
        arrayListsSecond = new ArrayList<>();
        components = new int[N];
        mark = new boolean[N];

        Arrays.fill(components, 0);
        Arrays.fill(mark, false);

        for (int i = 0; i < N; i++) {
            arrayLists.add(new ArrayList<>());
            arrayListsSecond.add(new ArrayList<>());
        }
        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                arrayLists.get(from).add(to);
                arrayListsSecond.get(to).add(from);
            }
        }

        for (int i = 0; i < N; i++) {
            if (!mark[i]) {
                dfs(i);
            }
        }

        Arrays.fill(mark, false);

        for (int i = ord.size() - 1; i >= 0; i--) {
            if (!mark[ord.get(i)]) {
                retdfs(ord.get(i));
                colour++;
            }
        }

        System.out.println(count());

    }

    private static int count() {
        int answer = 0;

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < arrayLists.get(i).size(); j++) {
                int colF = components[i];
                int colS = components[arrayLists.get(i).get(j)];
                Pair pair = new Pair(colF, colS);
                if (colF != colS && !set.contains(pair)) {
                    set.add(pair);
                    answer++;
                }
            }
        }

        return answer;
    }

    private static void dfs(int root) {
        mark[root] = true;
        for (int i = 0; i < arrayLists.get(root).size(); i++) {
            int to = arrayLists.get(root).get(i);
            if (!mark[to]) {
                dfs(to);
            }
        }
        ord.add(root);
    }

    private static void retdfs(int root) {
        mark[root] = true;
        for (int i = 0; i < arrayListsSecond.get(root).size(); i++) {
            int to = arrayListsSecond.get(root).get(i);
            if (!mark[to]) {
                retdfs(to);
            }
        }
        components[root] = colour;
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
