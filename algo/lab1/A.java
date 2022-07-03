import java.io.*;
import java.util.*;

public class A {
    private static int N;
    private static int M;
    private static ArrayList<ArrayList<Integer>> arrayLists;
    private static int[] mark;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            N = scanner.getNextInt();
            M = scanner.getNextInt();
        }
        arrayLists = new ArrayList<>();
        mark = new int[N];
        for (int i = 0; i < N; i++) {
            arrayLists.add(new ArrayList<>());
        }
        int[] in = new int[N];
        for (int i = 0; i < M; i++) {
            if (scanner.hasLine()) {
                int from = scanner.getNextInt();
                int to = scanner.getNextInt();
                in[to - 1]++;
                arrayLists.get(from - 1).add(to - 1);
            }
        }
        if (findCycle()) {
            System.out.println(-1);
        } else {
            StringBuilder stringBuilder = new StringBuilder();
            ArrayDeque<Integer> queue = new ArrayDeque<>();
            for (int i = 0; i < N; i++) {
                if (in[i] == 0) {
                    queue.addLast(i);
                }
            }
            for (int i = 0; i < N; i++) {
                int x = queue.pop();
                stringBuilder.append(x + 1).append(" ");
                for (int j = 0; j < arrayLists.get(x).size(); j++) {
                    int y = arrayLists.get(x).get(j);
                    in[y]--;
                    if (in[y] == 0) {
                        queue.addLast(y);
                    }
                }
            }
            System.out.println(stringBuilder.toString());
        }
    }


    private static boolean findCycle() {
        Arrays.fill(mark, 0);
        for (int i = 0; i < N; i++) {
            if (mark[i] == 0 && findCycle(i)) {
                return true;
            }
        }
        return false;
    }

    private static boolean findCycle(int i) {
        mark[i] = 1;
        for (int j = 0; j < arrayLists.get(i).size(); j++) {
            int y = arrayLists.get(i).get(j);
            if (mark[y] == 0 && findCycle(y)) {
                mark[i] = 2;
                return true;
            } else if (mark[y] == 1) {
                mark[i] = 2;
                return true;
            }
        }
        mark[i] = 2;
        return false;
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
