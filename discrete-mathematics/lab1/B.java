import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class B {
    private static int size;
    private static boolean[][] matrix;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        matrix = new boolean[size][size];

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                String line = scanner.getLine();
                for (int j = 0; j < i; j++) {
                    matrix[i][j] = matrix[j][i] = '1' == line.charAt(j);
                }
            }
        }

        List<Integer> answer = findHamiltonCycle();

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        int first = answer.get(answer.size() - 1);
        for (int i = first; i < size + first; i++) {
            writer.write((answer.get(i) + 1) + " ");
        }
        writer.flush();
    }

    private static List<Integer> findHamiltonCycle() {
        ArrayList<Integer> queue = new ArrayList<>(size * size);
        int first = 0;

        for (int i = 0; i < size; i++) {
            queue.add(i);
        }

        for (int i = 0; i < size * (size - 1); i++) {
            if (!matrix[queue.get(first)][queue.get(first + 1)]) {
                int j = first + 2;
                while (j < queue.size() - 1 && (!matrix[queue.get(first)][queue.get(j)] || (!matrix[queue.get(first + 1)][queue.get(j + 1)]))) {
                    j++;
                }
                if (queue.size() - 1 == j) {
                    j = 2 + first;
                    while (j < queue.size() && !matrix[queue.get(first)][queue.get(j)]) {
                        j++;
                    }
                }
                reverse( queue, 1 + first, j);
            }
            queue.add(queue.get(first));
            first++;
        }

        queue.add(first);
        return queue;
    }

    private static void reverse(ArrayList<Integer> list, int from, int to) {
        for (int i = 0; i < (to - from + 1) / 2; i++) {
            int left = list.get(from + i);
            list.set(from + i, list.get(to - i));
            list.set(to - i, left);
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
