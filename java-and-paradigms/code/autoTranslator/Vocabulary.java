package autoTranslator;

import autoTranslator.token.Tokonizer;

import java.io.*;
import java.util.LinkedHashMap;
import java.util.Map;

public class Vocabulary {
    private String filename;
    private Map<String, Map<Tokonizer, String>> vocabulary; //Map<first word, Map<all words, translation>>

    public Vocabulary(String filename) {
        if (filename.equals("")) {
            this.filename = filename;
        } else {
            this.filename = "autoTranslator.Vocabulary.txt";
        }
        vocabulary = new LinkedHashMap<>();
        readVocabulary();
    }

    private void readVocabulary() {
        try {
            Scanner scanner = new Scanner(filename);
            try {
                while (scanner.hasLine()) {
                    String string = scanner.getLine();
                    lineProcessing(string);
                }
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("!");
            System.err.println("Input File not found " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Input error: " + e.getMessage());
        }
    }

    private void lineProcessing(String string) {
        int beginIndex = 0;
        int endIndex = 0;
        while (string.charAt(endIndex) != '|') {
            endIndex++;
        }
        String word = string.substring(beginIndex, endIndex - 1).toLowerCase(); // Because there also whitespace
        Tokonizer tokonizer = new Tokonizer();
        tokonizer.tokenize(word);
        beginIndex = endIndex + 2;
        endIndex = string.length();
        if (Character.isWhitespace(string.charAt(endIndex - 1))) {
            endIndex--;
        }
        String translation = string.substring(beginIndex, endIndex).toLowerCase();
        if (vocabulary.get(tokonizer.next().getValue()) == null) {
            vocabulary.put(tokonizer.current().getValue(), new LinkedHashMap<>());
        }
        vocabulary.get(tokonizer.current().getValue()).put(tokonizer.toBegin(), translation);
    }

    public Map<String, Map<Tokonizer, String>> getVocabulary() {
        return vocabulary;
    }
}
