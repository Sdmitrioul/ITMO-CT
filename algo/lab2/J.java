import java.io.*;
import java.util.*;

public class J {
    private static int size;
    private static int rulesSize;
    private static int[] grandi;
    private static ArrayList<LinkedList<Integer>> graph;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
            rulesSize = scanner.getNextInt();
        }

        graph = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            graph.add(new LinkedList<>());
        }

        for (int i = 0; i < rulesSize; i++) {
            if (scanner.hasLine()) {
                graph.get(scanner.getNextInt() - 1).add(scanner.getNextInt() - 1);
            }
        }

        grandi();

        for (int i : grandi) {
            System.out.printf("%d\n", i);
        }
    }

    private static void grandi() {
        grandi = new int[size];
        Arrays.fill(grandi, -1);
        for (int i = 0; i < size; i++) {
            if (grandi[i] == -1) {
                mex(i);
            }
        }
    }

    private static void mex(int parent) {
        HashSet<Integer> set = new HashSet<>();
        Iterator<Integer> iterator = graph.get(parent).iterator();
        while (iterator.hasNext()) {
            int vertex = iterator.next();
            if (grandi[vertex] == -1) {
                mex(vertex);
            }
            set.add(grandi[vertex]);
        }
        for (int i = 0; i < size; i++) {
            if (!set.contains(i)) {
                grandi[parent] = i;
                break;
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
