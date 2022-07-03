import javax.swing.*;
import java.io.*;
import java.util.*;

public class I {
    private final static int INF = Integer.MAX_VALUE;
    private static int N;
    private static int[][] coordinates;
    private static boolean[] mark;
    private static int[] min;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
        }
        coordinates = new int[N][2];
        mark = new boolean[N];
        min = new int[N];
        Arrays.fill(min, INF);
        Arrays.fill(mark, false);
        for (int i = 0; i < N; i++) {
            if (scanner.hasLine()) {
                coordinates[i][0] = scanner.getNextInt();
                coordinates[i][1] = scanner.getNextInt();
            }
        }

        double answer = 0;

        min[0] = 0;

        for (int i = 0; i < N; i++) {
            int v = -1;
            for (int j = 0; j < N; j++) {
                if (!mark[j] && (v == -1 || min[j] < min[v])) {
                    v = j;
                }
            }
            mark[v] = true;
            //System.out.println(v);
            answer += Math.sqrt(min[v]);
            for (int j = 0; j < N; j++) {
                int l = length(coordinates[v][0], coordinates[v][1], coordinates[j][0], coordinates[j][1]);
                if (l < min[j]/* && i != j*/) {
                    min[j] = l;
                }
            }
        }

        System.out.println(answer);

    }

    private static int length(int x, int y, int x1, int y1) {
        return ((x - x1) * (x - x1) + (y - y1) * (y - y1));
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
