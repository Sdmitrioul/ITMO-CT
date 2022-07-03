import com.sun.jdi.ArrayReference;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class H {
    private static final String FIRST = "First player wins";
    private static final String SECOND = "Second player wins";
    private static int N;
    private static int M;
    private static int[] out;
    private static ArrayList<LinkedList<Integer>> graph;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner("game.in");
        //Scanner scanner = new Scanner(System.in);

        int start = 0;

        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
            start = scanner.getNextInt() - 1;
        }

        out = new int[N];
        graph = new ArrayList<>(N);

        for (int i = 0; i < N; i++) {
            graph.add(new LinkedList<>());
        }

        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt() - 1;
                int to = scanner.getNextInt() - 1;
                out[from]++;
                graph.get(to).add(from);
            }
        }

        int[] ans = winner();

        PrintWriter writer = new PrintWriter("game.out");
        writer.write(ans[start] == 1 ? FIRST : SECOND);
        writer.flush();
        writer.close();
        //System.out.println(Arrays.toString(ans));
        //System.out.println(ans[start] == 1 ? FIRST : SECOND);
    }

    private static int[] winner() {
       int[] ans = new int[N];
       int[] outLocal = new int[N];

       Queue<Integer> queue = new LinkedList<>();

       for (int i = 0; i < N; i++) {
           if (out[i] == 0) {
               ans[i] = -1;
               queue.add(i);
           }
       }

       while (!queue.isEmpty()) {
           int vertex = queue.remove();
           Iterator<Integer> iterator = graph.get(vertex).iterator();
           if (ans[vertex] == -1) {
               while (iterator.hasNext()) {
                   int u = iterator.next();
                   if (ans[u] == 0) {
                       ans[u] = 1;
                       queue.add(u);
                   }
               }
           } else {
               while (iterator.hasNext()) {
                   int u = iterator.next();
                   outLocal[u]++;
                   if (outLocal[u] == out[u]) {
                       ans[u] = -1;
                       queue.add(u);
                   }
               }
           }
       }
       return ans;
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
