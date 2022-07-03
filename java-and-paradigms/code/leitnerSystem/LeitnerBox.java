package leitnerSystem;

import java.io.*;
import java.util.*;
import java.util.List;

public class LeitnerBox {
    private Map<String, String> words; //word - translation
    private String filename;
    private Random random;

    public LeitnerBox(String filename) {
        this.filename = filename;
        words = new HashMap<>();
        putWordsOutOfFile(filename);
        random = new Random();
    }

    public LeitnerBox() {
        words = new HashMap<>();
        random = new Random();
    }

    private void putWordsOutOfFile(String filename) {
        try {
            Scanner scanner = new Scanner(filename);
            try {
                while (scanner.hasLine()) {
                    String string = scanner.getLine();
                    putWordIntoBox(string);
                }
            } catch (IOException e) {
                System.out.println("Something goes wrong, file was damaged");
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("There's miss information, file was deleted");
        } catch (UnsupportedEncodingException e) {
            System.out.println("There's encoding exception" + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong");
        }
    }

    public boolean checkAnswear(String word, String translation) {
        return words.get(word).equals(translation.toLowerCase());
    }

    public String moveWordFromBox(String word) {
        StringBuilder stringBuilder = new StringBuilder(word);
        stringBuilder.append("-").append(words.get(word));
        words.remove(word);
        return stringBuilder.toString();
    }

    public boolean isEmpty() {
        return words.isEmpty();
    }

    public int size() {
        return words.size();
    }

    public void putWordIntoBox(String string) {
        int beginIndex = 0;
        while (Character.isWhitespace(string.charAt(beginIndex))) {
            beginIndex++;
        }
        int endindex = beginIndex;
        while (string.charAt(endindex) != '-') {
            endindex++;
        }
        endindex--;
        int tmp = endindex + 2;
        while (Character.isWhitespace(string.charAt(endindex))) {
            endindex--;
        }
        String word = string.substring(beginIndex, ++endindex).toLowerCase();
        beginIndex = tmp;
        while (Character.isWhitespace(string.charAt(beginIndex))) {
            beginIndex++;
        }
        endindex = string.length() - 1;
        while (Character.isWhitespace(string.charAt(endindex))) {
            endindex--;
        }
        String translation = string.substring(beginIndex, ++endindex).toLowerCase();
        words.put(word, translation);
    }

    public String getRandomWord() {
        List list = new ArrayList(words.keySet());
        return list.get(random.nextInt(words.size())).toString();
    }

    public void close(int a) {
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("LeitnerBox_" + a + ".txt"), "utf8"));
            try {
                StringBuilder stringBuilder = new StringBuilder();
                for (Map.Entry entry : words.entrySet()) {
                    stringBuilder.append(entry.getKey()).append("-").append(entry.getValue()).append('\n');
                }
                writer.write(stringBuilder.toString());
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }
}
