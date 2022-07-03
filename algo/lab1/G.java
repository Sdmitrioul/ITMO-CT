import java.io.*;
import java.util.*;

public class G {
    private static HashMap<String, Integer> names = new HashMap<>();
    private static HashMap<Integer, String> toNames = new HashMap<>();
    private static int N;
    private static int M;
    private static ArrayList<ArrayList<Integer>> E, RE;
    private static boolean[] mark;
    private static ArrayList<Integer> order = new ArrayList<>();
    private static int colour = -1;
    private static int[] colours;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }
        E = new ArrayList<>();
        RE = new ArrayList<>();
        mark = new boolean[2 * N];
        for (int i = 0; i < 2 * N; i++) {
            E.add(new ArrayList<>());
            RE.add(new ArrayList<>());
        }
        int tmp = 0;
        for (int i = 0; i < N; i++) {
            if (scanner.hasLine()){
                String name = scanner.getNextWord();
                names.put("+" + name, tmp);
                toNames.put(tmp++, "+" + name);
                names.put("-" + name, tmp);
                toNames.put(tmp++, "-" + name);
            }
        }

        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                String first = scanner.getNextWord();
                String second = scanner.getNextWord();
                E.get(names.get(first)).add(names.get(second));
                E.get(names.get(negate(second))).add(names.get(negate(first)));
                RE.get(names.get(second)).add(names.get(first));
                RE.get(names.get(negate(first))).add(names.get(negate(second)));
            }
        }

        for (int i = 0; i < 2 * N; i++) {
            if (!mark[i]) {
                dfs(i);
            }
        }

        colours = new int[2 * N];
        Arrays.fill(colours, -1);
        Collections.reverse(order);

        for (int i = 0; i < 2 * N; i++) {
            if (colours[order.get(i)] == -1) {
                rdfs(order.get(i), ++colour);
            }
        }

        ArrayList<String> answer = new ArrayList<>();
        boolean checker = true;

        for (int i = 0; i < N; i++) {
            if (colours[2 * i] == colours[2 * i + 1]) {
                checker = false;
                break;
            } else if (colours[2 * i] > colours[2 * i + 1]) {
                answer.add(colours[2 * i] > colours[2 * i + 1] ? toNames.get(2 * i) : toNames.get(2 * i + 1));
            }
        }

        if (!checker) {
            System.out.println(-1);
        } else {
            System.out.println(answer.size());
            for (String ans : answer) {
                System.out.println(ans.substring(1));
            }
        }


    }

    private static void dfs(int root) {
        mark[root] = true;
        for (int i = 0; i < E.get(root).size(); i++) {
            int to = E.get(root).get(i);
            if (!mark[to]) {
                dfs(to);
            }
        }
        order.add(root);
    }

    private static void rdfs(int root, int colour) {
        colours[root] = colour;
        for (int i = 0; i < RE.get(root).size(); i++) {
            int to = RE.get(root).get(i);
            if (colours[to] == -1) {
                rdfs(to, colour);
            }
        }
    }

    private static boolean isNegative(String name) {
        return '-' == name.charAt(0);
    }

    private static String negate(String name) {
        return isNegative(name) ? "+" + name.substring(1) : '-' + name.substring(1);
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
            return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION ||  c == '\'' || c == '+' || c == '-';
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
