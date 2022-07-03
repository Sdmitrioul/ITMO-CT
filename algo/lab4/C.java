import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.IntStream;

public class C {
    private static int n, m;
    private static boolean[] used, boys, girls;
    private static int[] dist;
    private static ArrayList<ArrayList<Integer>> graph;

    public static void main(String[] args) {
        String answer = "";
        try (final Scanner scanner = new Scanner(System.in)) {
            try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
                if (scanner.hasLine()) {
                    int count = scanner.getNextInt();
                    while (count-- > 0) {
                        read(scanner);
                        answer = process();
                        writer.write(answer + "\n");
                    }
                }
            } catch (IOException ex) {
                System.err.println("Writing goes wrong");
            }
        } catch (IOException e) {
            System.err.println("Reading goes wrong");
        }
    }

    private static String process() {
        for (int i  = 0; i < n; i++) {
            Arrays.fill(used, false);
            dfs(i);
        }

        Arrays.fill(used, false);

        for (int i = 0; i < m; i++) {
            if (dist[i] != -1) {
                used[dist[i]] = true;
            }
        }

        boys = new boolean[n];
        girls = new boolean[m];

        for (int i = 0; i < n; i++) {
            if (!used[i]) {
                dfsNew(i);
            }
        }

        List<Integer> men = new ArrayList<>();
        List<Integer> women = new ArrayList<>();

        for (int i = 0; i < n; i++) {
            if (boys[i]) {
                men.add(i);
            }
        }

        for (int i = 0; i < m; i++) {
            if (!girls[i]) {
                women.add(i);
            }
        }

        StringBuilder answer = new StringBuilder();
        answer.append(men.size() + women.size()).append("\n");
        answer.append(men.size()).append(" ").append(women.size()).append("\n");
        for (int i : men) {
            answer.append(i + 1).append(" ");
        }
        answer.append("\n");
        for (int i : women) {
            answer.append(i + 1).append(" ");
        }
        answer.append("\n");
        return answer.toString();
    }

    private static boolean dfsNew(int i) {
        if (boys[i]) {
            return false;
        }

        boys[i] = true;

        for (var girl : graph.get(i)) {
            if (!girls[girl]) {
                girls[girl] = true;
                if (dist[girl] != -1) {
                    dfsNew(dist[girl]);
                }
            }
        }

        return true;
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

    private static void read(Scanner scanner) throws IOException {
        n = m = 0;
        graph = new ArrayList<>();

        if (scanner.hasLine()) {
            n = scanner.getNextInt();
            m = scanner.getNextInt();
        }

        used = new boolean[n];
        dist = new int[m];

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        Arrays.fill(used, false);
        Arrays.fill(dist, -1);

        for (int i = 0; i < n; i++) {
            if (scanner.hasLine()) {
                NavigableSet<Integer> set = new TreeSet<>();
                IntStream stream = IntStream.range(0, m);
                stream.forEach(set::add);
                int vertex = scanner.getNextInt();
                while (vertex != 0) {
                    set.remove(vertex - 1);
                    vertex = scanner.getNextInt();
                }
                graph.get(i).addAll(set);
            }
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
