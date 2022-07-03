package bbCodeToHtml;

import bbCodeToHtml.parse.Parser;
import bbCodeToHtml.parse.ParserBB;
import bbCodeToHtml.token.Tokenizer;

import java.io.*;

public class BbCodeToHTML {
    public static void main(String[] args) {
        Parser parser = new ParserBB();
        writeInFile(parser.parse(readFromFile(args[0])), args[0]);
    }

    private static Tokenizer readFromFile(String filename) {
        Tokenizer tokenizer = new Tokenizer();
        try {
            Scanner scanner = new Scanner(filename);
            try {
                while (scanner.hasLine()) {
                    tokenizer.tokenize(scanner.getLine());
                    tokenizer.addNewLine();
                }
                tokenizer.deleteLast();
                tokenizer.addEnd();
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong: " + e.getMessage());
        }
        return tokenizer;
    }

    private static void writeInFile(String string, String filename) {
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("BBcodeToHTML-" + filename), "utf8"));
            try {
                writer.write(string);
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }
}
