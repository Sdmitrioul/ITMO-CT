package autoTranslator;

import autoTranslator.token.Tokonizer;

import java.io.*;
import java.util.Map;

public class Translator {
    private Map<String, Map<Tokonizer, String>> vocabulary; //Map<first word, Map<all words, translation>>
    private String translatingFile;

    public Translator(String vocabularyFile, String translatingFile) {
        this.translatingFile = translatingFile;
        Vocabulary vocabulary = new Vocabulary(vocabularyFile);
        this.vocabulary = vocabulary.getVocabulary();
    }

    public void translate() {
        String text;
        Tokonizer tokonizer = new Tokonizer();
        try {
            Scanner scanner = new Scanner(translatingFile);
            try {
                while (scanner.hasLine()) {
                    String string = scanner.getLine();
                    tokonizer.tokenize(string);
                    tokonizer.addNewLine();
                }
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("File not found : " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Some problem : " + e.getMessage());
        }
        Parser parser = new Parser(tokonizer, vocabulary);
        text = parser.getText();
        writeFile(text);
    }

    private void writeFile(String translation) {
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("Translated" + translatingFile), "utf8"));
            try {
                writer.write(translation);
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }
}
