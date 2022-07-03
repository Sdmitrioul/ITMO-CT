import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class B {
    private static int[] dsu, size;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner("destroy.in");
        int countVertex = 0;
        int countEdges = 0;
        long cost = 0;

        if (scanner.hasLine()) {
            countVertex = scanner.getNextInt();
            countEdges = scanner.getNextInt();
            cost = scanner.getNextLong();
        }

        ArrayList<Edge> list = new ArrayList<>(countEdges);

        for (int i = 0; i < countEdges; i++) {
            if (scanner.hasLine()) {
                int first = scanner.getNextInt() - 1;
                int second = scanner.getNextInt() - 1;
                long weight = scanner.getNextLong();
                list.add(new Edge(i + 1, first, second, weight));
            }
        }

        size = new int[countVertex];
        dsu = new int[countVertex];
        Arrays.fill(size, 1);
        Collections.sort(list);

        for (int i = 0; i < countVertex; i++) {
            dsu[i] = i;
        }

        Set<Integer> set = new HashSet<>();

        for (int i = 0; i < countEdges; i++) {
            Edge edge = list.get(i);
            if (findDsu(edge.firstVertex) != findDsu(edge.secondVertex)) {
                set.add(edge.number);
                unionDsu(edge.firstVertex, edge.secondVertex);
            }
        }

        ArrayList<Integer> answer = new ArrayList<>();

        for (int i = list.size() - 1; i >= 0; i--) {
            Edge edge = list.get(i);
            if (cost < edge.weight) {
                break;
            }
            if (!set.contains(edge.number)) {
                answer.add(edge.number);
                cost -= edge.weight;
            }
        }

        Collections.sort(answer);

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("destroy.out"), StandardCharsets.UTF_8));
        writer.write(answer.size() + " ");
        writer.newLine();
        for (int i : answer) {
            writer.write(i + " ");
        }
        writer.flush();
    }

    private static int findDsu(int v) {
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

    private static class Edge implements Comparable<Edge>{
        final int number;
        final int firstVertex;
        final int secondVertex;
        final long weight;

        public Edge(int number, int firstVertex, int secondVertex, long weight) {
            this.number = number;
            this.firstVertex = firstVertex;
            this.secondVertex = secondVertex;
            this.weight = weight;
        }

        @Override
        public int compareTo(Edge o) {
            return Long.compare(o.weight, this.weight);
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

        public long getNextLong() {
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
            return Long.parseLong(line.substring(begin, index));
        }
    }
}
