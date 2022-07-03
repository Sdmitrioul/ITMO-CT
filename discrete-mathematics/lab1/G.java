import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class G {
    private static ArrayList<ArrayList<Integer>> graph;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        int edgeSize = 0;
        int vertexSize = 0;
        graph = new ArrayList<>();

        if (scanner.hasLine()) {
            vertexSize = scanner.getNextInt();
            edgeSize = scanner.getNextInt();
        }

        int[] color = new int[vertexSize];
        Arrays.fill(color, -1);

        for (int i = 0; i < vertexSize; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < edgeSize; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                graph.get(from).add(to);
                graph.get(to).add(from);
            }
        }
        boolean[] used = new boolean[vertexSize];

        int max = 0;

        for (List list : graph) {
            max = Math.max(max, list.size());
        }

        max = max % 2 == 0 ? max + 1 : max;

        PriorityQueue<Pair> queue = new PriorityQueue<>();
        for (int i = 0; i < graph.size(); i++) {
            queue.add(new Pair(i, graph.get(i).size()));
        }
        while (!queue.isEmpty()) {
            int vertex = queue.remove().first;
            HashSet<Integer> set = new HashSet<>();
            for (int i : graph.get(vertex)) {
                set.add(color[i]);
            }
            color[vertex] = mex(set);
        }

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));
        writer.write(max + "\n");
        for (int i : color) {
            writer.write((i + 1) + "\n");
        }
        writer.flush();
    }

    private static int mex(Set<Integer> set) {
        int i = 0;
        while (set.contains(i)) {
            i++;
        }
        return i;
    }

    private static class Pair implements Comparable, Comparator{
        final int first;
        final int second;

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }

        @Override
        public int compare(Object o1, Object o2) {
            Pair first = (Pair) o1;
            Pair second = (Pair) o2;
            return Integer.compare(second.second, first.second);
        }

        @Override
        public int compareTo(Object o) {
            Pair first = (Pair) o;
            return Integer.compare(first.second, this.second);
        }

        @Override
        public String toString() {
            return first + " " + second;
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





/*
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class G {
    private static int size;
    private static int rulesSize;
    private static int[] grandi;
    private static int max = 0;
    private static ArrayList<ArrayList<Integer>> graph;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
            rulesSize = scanner.getNextInt();
        }

        graph = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            graph.add(new ArrayList<>());
        }

        for (int i = 0; i < rulesSize; i++) {
            if (scanner.hasLine()) {
                graph.get(scanner.getNextInt() - 1).add(scanner.getNextInt() - 1);
            }
        }

        grandi();
        max++;

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));
        writer.write(max % 2 == 0 ? (max + 1) + "\n" : max + "\n");
        for (int i : grandi) {
            writer.write((i + 1) + "\n");
        }
        writer.flush();
    }

    private static void grandi() {
        grandi = new int[size];
        Arrays.fill(grandi, -1);
        for (int i = 0; i < size; i++) {
            if (grandi[i] == -1) {
                mex(i);
            }
        }
    }

    private static void mex(int parent) {
        HashSet<Integer> set = new HashSet<>();
        for (int vertex : graph.get(parent)) {
            if (grandi[vertex] == -1) {
                mex(vertex);
            }
            set.add(grandi[vertex]);
        }
        int i = 0;
        while (set.contains(i)) {
            i++;
        }
        grandi[parent] = i;
        max = Math.max(i, max);
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
*/
