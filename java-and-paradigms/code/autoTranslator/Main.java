package autoTranslator;

import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        String vocabularyFile = "";
        if (args.length != 0)
            vocabularyFile = args[0];
        System.out.println("Write file name");
        String filename = "";
        try {
            Scanner scanner = new Scanner(System.in);
            try {
                if (scanner.hasLine())
                    filename = scanner.getLine();
            } finally {
                scanner.close();
            }
        } catch (IOException e) {
            System.out.println("There some problem : " + e.getMessage());
        }
        Translator translator = new Translator(vocabularyFile, filename);
        translator.translate();
    }
}
