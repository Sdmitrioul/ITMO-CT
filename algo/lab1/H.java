import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class H {
    private static int N;
    private static int[][] matrix;
    private static boolean[] mark;
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner("avia.in");
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
        }

        matrix = new int[N][N];
        mark = new boolean[N];

        for (int i = 0; i < N; i++) {
            if (scanner.hasLine()) {
                for (int j = 0; j < N; j++) {
                    matrix[i][j] = scanner.getNextInt();
                }
            }
        }

        Arrays.fill(mark, false);

        int left = -1;
        int right = 1000000001;
        while (right - left > 1) {
            int mid = (left + right) / 2;

            if (check(mid)) {
                right = mid;
            } else {
                left = mid;
            }
        }

        //System.out.println(right);
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("avia.out"), StandardCharsets.UTF_8));
        writer.write(Integer.toString(right));
        writer.flush();
        writer.close();
    }

    private static boolean check(int mas) {
        int ch = 0;
        Arrays.fill(mark, false);
        for (int i = 0; i < N; i++) {
            if (!mark[i]) {
                ch++;
                dfs(i, mas);
            }
        }

        if (ch > 1) return false;

        ch = 0;
        Arrays.fill(mark, false);

        for (int i = 0; i < N; i++) {
            if (!mark[i]) {
                ch++;
                rdfs(i, mas);
            }
        }

        return ch <= 1;
    }

    private static void dfs(int root, int mas) {
        mark[root] = true;
        for (int i = 0; i < N; i++) {
            if (matrix[root][i] <= mas && !mark[i]) {
                dfs(i, mas);
            }
        }
    }

    private static void rdfs(int root, int mas) {
        mark[root] = true;
        for (int i = 0; i < N; i++) {
            if (matrix[i][root] <= mas && !mark[i]) {
                rdfs(i, mas);
            }
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
