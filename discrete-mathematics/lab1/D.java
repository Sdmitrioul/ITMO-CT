import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class D {
    private static Scanner scanner;
    private static boolean[][] matrix;
    public static void main(String[] args) throws IOException {
        scanner = new Scanner(System.in);
        int size = 0;

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        matrix = new boolean[size][size];

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                String line = scanner.getLine();
                for (int j = 0; j < i; j++) {
                    if ('1' == line.charAt(j)) {
                        matrix[i][j] = true;
                    } else {
                        matrix[j][i] = true;
                    }
                }
            }
        }

        List<Integer> array = new ArrayList<>(size);

        for (int i = 0; i < size; i++) {
            array.add(i + 1);
        }

        sort(array, 0, size - 1);
        while (!matrix[array.get(size - 1) - 1][array.get(0) - 1]) {
            Collections.shuffle(array);
            sort(array, 0, size - 1);
        }

        for (int i = 0; i < size; i++) {
            System.out.print(array.get(i) + " ");
        }
    }

    private static void shuffle(int[] array) {
        Random generator = new Random();
        for (int i = 0; i < array.length; i++) {
            int random = generator.nextInt(array.length);
            int tmp = array[i];
            array[i] = array[random];
            array[random] = tmp;
        }
    }

    private static void sort(List<Integer> arr, int l, int r) throws IOException {
        if (l < r) {
            int m = (l + r) / 2;

            sort(arr, l, m);
            sort(arr, m + 1, r);

            merge(arr, l, m, r);
        }
    }

    private static void merge(List<Integer> arr, int l, int m, int r) throws IOException {
        int n1 = m - l + 1;
        int n2 = r - m;
        int[] leftArr = new int[n1];
        int[] rightArr = new int[n2];

        for (int i = 0; i < n1; ++i) {
            leftArr[i] = arr.get(l + i);
        }
        for (int j = 0; j < n2; ++j) {
            rightArr[j] = arr.get(m + 1 + j);
        }

        int i = 0, j = 0;
        int k = l;
        while (i < n1 && j < n2) {
            if (compare(leftArr[i], rightArr[j])) {
                arr.set(k, leftArr[i]);
                i++;
            }
            else {
                arr.set(k, rightArr[j]);
                j++;
            }
            k++;
        }

        while (i < n1) {
            arr.set(k, leftArr[i]);
            i++;
            k++;
        }

        while (j < n2) {
            arr.set(k, rightArr[j]);
            j++;
            k++;
        }
    }

    private static boolean compare(int i, int j) throws IOException {
        return matrix[i - 1][j - 1];
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
