import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class A {
    private final static String BAD = "No\n";
    private final static String GOOD = "Yes\n";

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        String string = "";

        if (scanner.hasLine()) {
            string = scanner.getLine();
        }

        MyString myString = new MyString(string, 31);
        //MyString myString2 = new MyString(string, 37);


        int size = 0;

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                int a = scanner.getNextInt();
                int b = scanner.getNextInt();
                int c = scanner.getNextInt();
                int d = scanner.getNextInt();
                boolean ans = myString.equalsSubstrings(a, b, c, d) ;//&& myString2.equalsSubstrings(a, b, c, d);
                writer.write(ans ? GOOD : BAD);
            }
        }

        writer.flush();
    }

    private static class MyString {
        private final String STRING;
        private final int SIMPLE_INT;
        //private final int MOD = 1_000_000_007;
        private long[] hash;
        private long[] pow;

        public MyString(String string, int simple) {
            this.STRING = string;
            this.SIMPLE_INT = simple;
            hash = new long[string.length() + 1];
            pow = new long[string.length() + 1];
            preCalculations();
        }

        private void preCalculations() {
            hash[0] = 0L;
            pow[0] = 1L;
            for (int i = 1; i <= STRING.length(); i++) {
                pow[i] = pow[i - 1] * SIMPLE_INT;
                hash[i] = hash[i - 1] + STRING.charAt(i - 1) * pow[i - 1];
            }
        }

        private long hashOfSubstring(int left, int right) {
            return hash[right] - hash[left];
        }

        public boolean equalsSubstrings(int leftFirst, int rightFirst, int leftSecond, int rightSecond) {
            int lengthFirst = rightFirst - leftFirst + 1;
            int lengthSecond = rightSecond - leftSecond + 1;
            if (lengthFirst != lengthSecond) {
                return false;
            } else {
                long left = leftFirst < leftSecond ? hashOfSubstring(leftFirst - 1, rightFirst) * pow[- leftFirst + leftSecond] : hashOfSubstring(leftFirst - 1, rightFirst);
                long right = leftFirst > leftSecond ? hashOfSubstring(leftSecond - 1, rightSecond) * pow[leftFirst - leftSecond] : hashOfSubstring(leftSecond - 1, rightSecond);
                //System.out.println(left + " " + right);
                return left == right;
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
