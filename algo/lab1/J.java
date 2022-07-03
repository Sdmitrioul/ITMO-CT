import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.NoSuchElementException;

public class J {
    private static ArrayList<Edge> E;
    private static int N;
    private static int M;
    private static int[] dsu, size;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }

        E = new ArrayList<>();

        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                int weight = scanner.getNextInt();
                E.add(new Edge(weight, from, to));
            }
        }

        size = new int[N];
        dsu = new int[N];
        Arrays.fill(size, 1);
        Collections.sort(E);
        for (int i = 0; i < N; i++) {
            dsu[i] = i;
        }

        long answer = 0;

        for (int i = 0; i < M; i++) {
            Edge edge = E.get(i);
            if (findDsu(edge.getFirst()) != findDsu(edge.getSecond())) {
                answer += edge.getWeight();
                unionDsu(edge.getFirst(), edge.getSecond());
            }
        }

        System.out.println(answer);
    }

    private static int findDsu (int v) {
        if (v == dsu[v])
            return v;
        return dsu[v] = findDsu(dsu[v]);
    }

    private static void unionDsu(int v, int p) {
        v = findDsu(v);
        p = findDsu(p);
        if (v != p) {
            if (size[v] < size[p]) {
                int tmp = v;
                v = p;
                p = tmp;
            }
            dsu[p] = v;
            size[v] += size[p];
        }
    }

    private static class Edge implements Comparable{
        private Integer  weight;
        private int first;
        private int second;

        public Edge(int weight, int first, int second) {
            this.weight = weight;
            this.first = first;
            this.second = second;
        }

        @Override
        public int compareTo(Object o) {
            Edge r = (Edge) o;
            return this.weight.compareTo(r.weight);
        }

        public Integer getWeight() {
            return weight;
        }

        public int getFirst() {
            return first;
        }

        public int getSecond() {
            return second;
        }

        @Override
        public String toString() {
            return "{ w: " + weight + "; f: " + first + "; s: " + second + "}";
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
