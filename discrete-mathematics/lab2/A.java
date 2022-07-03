import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class A {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner("schedule.in");
        int size = 0;
        ArrayList<Pair> list = new ArrayList<>();

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        TreeSet<Integer> set = new TreeSet<>();

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                int time = scanner.getNextInt();
                int cost = scanner.getNextInt();
                list.add(new Pair(time, cost));
                set.add(i + 1);
            }
        }

        Collections.sort(list);
        long ans = 0;

        for (int i = 0; i < size; i++) {
            Pair task = list.get(i);
            if (task.time > size) {
                continue;
            }
            Integer time = set.floor(task.time);
            if (time != null && time <= task.time) {
                set.remove(time);
                continue;
            }
            if (time == null) {
                ans += task.cost;
            }
        }
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("schedule.out"), StandardCharsets.UTF_8));
        writer.write(ans + " ");
        writer.flush();
    }

    private static class Pair implements Comparable<Pair> {
        final int time;
        final int cost;

        public Pair(int time, int cost) {
            this.time = time;
            this.cost = cost;
        }


        @Override
        public int compareTo(Pair o) {
            return Integer.compare(o.cost, this.cost);
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
