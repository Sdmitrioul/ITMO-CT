import java.io.*;
import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class M {
    private static long[] pow;
    private static String strings[];
    private static final int SIMPLE_NUMBER = 31;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        int size = 2;
        int maxLength = Integer.MAX_VALUE;
        int max = 0;
        int position = 0;

        strings = new String[size];

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                strings[i] = scanner.getLine();
                if (maxLength > strings[i].length()) {
                    position = i;
                    maxLength = strings[i].length();
                }
                max = Math.max(max, strings[i].length());
            }
        }

        preCalc(max);
        positionMinimum(position);

        List<Integer> answer = new ArrayList<>();
        int left = 0;
        int right = maxLength + 1;

        while (right - left > 1) {
            int middle = (left + right) / 2;
            List<Integer> ans = findCommonSubstrings(middle);

            if (ans.size() > 0) {
                answer = ans;
                left = middle;
            } else {
                right = middle;
            }
        }

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        if (answer.size() == 0) {
            if (size == 1) {
                writer.write(strings[0]);
            } else {
                writer.write("");
            }
        } else {
            ArrayList<String> answers = new ArrayList<>();
            for (int i : answer) {
                answers.add(strings[strings.length - 1].substring(i - left + 1, i + 1));
            }
            Collections.sort(answers);
            writer.write(answers.get(0));
        }

        /*3
        barabababa
        baababababara
        babara

        3
abbbabababaaba
abbbafbbababaaba
abbbafbababaaba
3
aaaaaaaka
akaa
aka*/

        writer.flush();
    }

    private static List<Integer> findCommonSubstrings(int substringLength) {
        HashSet<Long> set = new HashSet<>();
        List<Integer> answer = new ArrayList<>();
        long[] hash = findSubstringsHash(strings[0], substringLength);

        for (int i = substringLength - 1; i < strings[0].length(); i++) {
            set.add(hash[i]);
        }

        for (int i = 1; i < strings.length; i++) {
            HashSet<Long> tmpSet = new HashSet<>();
            hash = findSubstringsHash(strings[i], substringLength);

            for (int j = substringLength - 1; j < strings[i].length(); j++) {
                if (set.contains(hash[j])) {
                    tmpSet.add(hash[j]);
                    if (i + 1 == strings.length) {
                        answer.add(j);
                    }
                }
            }
            set = tmpSet;
        }

        return answer;
    }

    private static long[] findSubstringsHash(String string, int substringLength) {
        long[] hash = new long[string.length()];
        hash[0] = string.charAt(0) - 'a' + 1;

        for (int i = 1; i < string.length(); i++) {
            if (i < substringLength) {
                hash[i] = hash[i - 1] * SIMPLE_NUMBER + string.charAt(i) - 'a' + 1;
            } else {
                hash[i] = (hash[i - 1] - (string.charAt(i - substringLength) - 'a' + 1) * pow[substringLength - 1]) * SIMPLE_NUMBER + string.charAt(i) - 'a' + 1;
            }
        }

        return hash;
    }

    private static void positionMinimum(int position) {
        String first = strings[0];
        strings[0] = strings[position];
        strings[position] = first;
    }

    private static void preCalc(int max) {
        pow = new long[max + 1];
        pow[0] = 1L;

        for (int i = 1; i <= max; i++) {
            pow[i] = pow[i - 1] * SIMPLE_NUMBER;
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
