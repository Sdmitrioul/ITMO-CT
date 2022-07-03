import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.NoSuchElementException;

public class F {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        int size = 0;

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        ArrayList<Pair> answer = new ArrayList<>(size - 1);
        int[] prufer = new int[size];
        int[] degree = new int[size + 2];

        Arrays.fill(degree, 1);

        if (scanner.hasLine()) {
            for (int i = 0; i < size - 2; i++) {
                int vertex = scanner.getNextInt() - 1;
                prufer[i] = vertex;
                degree[vertex]++;
            }
        }

        int leaf = -1;

        for (int i = 0; i < size; i++) {
            if (degree[i] == 1) {
                leaf = i;
                break;
            }
        }

        int tmpLeaf = leaf;

        for (int i = 0; i + 2 < size; i++) {
            int vertex = prufer[i];
            answer.add(new Pair(leaf, vertex));
            degree[leaf]--;
            degree[vertex]--;
            if (degree[vertex] == 1 && vertex < tmpLeaf) {
                leaf = vertex;
                continue;
            }
            tmpLeaf++;
            while (degree[tmpLeaf] != 1 && tmpLeaf < size) {
                tmpLeaf++;
            }
            leaf = tmpLeaf;
        }

        for (int i = 0; i + 1 < size; i++) {
            if (degree[i] == 1) {
                answer.add(new Pair(i, size - 1));
            }
        }

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        for (Pair ans : answer) {
            writer.write(ans.toString());
            writer.newLine();
        }

        writer.flush();
    }

    private static class Pair {
        private final int vertexFirst;
        private final int vertexSecond;

        public Pair(int vertexFirst, int vertexSecond) {
            this.vertexFirst = vertexFirst;
            this.vertexSecond = vertexSecond;
        }

        public int getVertexFirst() {
            return vertexFirst;
        }

        public int getVertexSecond() {
            return vertexSecond;
        }

        @Override
        public String toString() {
            return (this.vertexFirst + 1) + " " + (this.vertexSecond + 1);
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
