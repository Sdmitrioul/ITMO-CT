import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class D {
    private static int n = 0;
    private static int MAX_SPEED = 0;
    private static ArrayList<UfoType> input = new ArrayList<>();
    private static boolean[] used;
    private static int[] dist;
    private static ArrayList<ArrayList<Integer>> graph;


    public static void main(String[] args) {
        try (final Scanner scanner = new Scanner(System.in)) {
            read(scanner);
            preProcess();
            String answer = "";
            answer = process();
            try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
                writer.write(answer);
            } catch (IOException ex) {
                System.err.println("Writing goes wrong");
            }
        } catch (IOException e) {
            System.err.println("Reading goes wrong");
        }
    }

    private static void preProcess() {
        graph = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
            UfoType firstUfo = input.get(i);
            for (int j = 0; j < n; j++) {
                if (i != j) {
                    UfoType secondUfo = input.get(j);
                    if (firstUfo.canBeSame(MAX_SPEED, secondUfo)) {
                        graph.get(i).add(j);
                    }
                }
            }
        }
    }

    private static String process() {
        used = new boolean[n];
        dist = new int[n];

        Arrays.fill(used, false);
        Arrays.fill(dist, -1);

        for (int i  = 0; i < n; i++) {
            Arrays.fill(used, false);
            dfs(i);
        }

        int counter = 0;

        for (int i = 0; i < n; i++) {
            if (dist[i] != -1) {
                counter++;
            }
        }

        Arrays.fill(used, false);

        return Integer.toString(n - counter);
    }

    private static void read(Scanner scanner) throws IOException {
        if (scanner.hasLine()) {
            n = scanner.getNextInt();
            MAX_SPEED = scanner.getNextInt();
        }
        for (int i = 0; i < n; i++) {
            if (scanner.hasLine()) {
                String time = scanner.getNextWord();
                int x = scanner.getNextInt();
                int y = scanner.getNextInt();
                input.add(new UfoType(time, x, y));
            }
        }
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

    private static class UfoType {
        final int time;
        final int x;
        final int y;

        public UfoType(String time, int x, int y) {
            String[] hm = time.split(":");
            this.time = Integer.parseInt(hm[0]) * 60 + Integer.parseInt(hm[1]);
            this.x = x;
            this.y = y;
        }

        public boolean canBeSame(int MaxSpeed, UfoType ufo) {
            double dist = Math.pow(this.x - ufo.x, 2) + Math.pow(this.y - ufo.y, 2);
            return Math.sqrt(dist) / MaxSpeed * 60 <= this.time - ufo.time;
        }

        @Override
        public String toString() {
            return "UfoType{" +
                    "time=" + time +
                    ", x=" + x +
                    ", y=" + y +
                    '}';
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
            return !Character.isWhitespace(c);
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
