package md2html;

import java.io.*;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;

public class Md2Html {
    private static final Map<String, String> BOUNDARIES = Map.of(
            "**", "<strong>",
            "__", "<strong>",
            "*", "<em>",
            "_", "<em>",
            "--", "<s>",
            "`", "<code>",
            "++", "<u>"
    );
    private static final Map<Character, String> SPESIALSYMB = Map.of(
            '<', "&lt;",
            '\\', "",
            '>', "&gt;",
            '&', "&amp;"
    );

    public static void main(String[] args) {
        //StringBuilder text = new StringBuilder();
        try {
            Scanner scanner = new Scanner(args[0], "utf8");
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf8"));
            try {
                while (scanner.hasLine()) {
                    StringBuilder paragraphBuilder = new StringBuilder();
                    String string = scanner.getLine();
                    while (string.length() > 0) {
                        paragraphBuilder.append(string).append("\n");
                        string = scanner.getLine();
                    }
                    if (paragraphBuilder.length() > 0) {
                        paragraphBuilder.deleteCharAt(paragraphBuilder.length() - 1);
                        StringBuilder paragraph = new StringBuilder();
                        for (int i = 0; i < paragraphBuilder.length(); i++) {
                            char symbol = paragraphBuilder.charAt(i);
                            String escape = SPESIALSYMB.get(symbol);
                            paragraph.append(escape != null ? escape : Character.toString(symbol));
                        }
                        //text.append(toHtml(paragraph.toString()));
                        writer.write(toHtml(paragraph.toString()));
                    }
                }
            } catch (IOException e) {
                System.out.println("Some problem: " + e.getMessage());
            } finally {
                scanner.close();
                writer.close();
            }
        } catch (UnsupportedEncodingException e) {
            System.out.println("Encoding error: " + e.getMessage());
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Some problem: " + e.getMessage());
        }
        /*try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf8"));
            try {
                writer.write(text.toString());
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.out.println("Writing problem: " + e.getMessage());
        }*/
    }

    private static String toHtml(String paragraph) {
        boolean isOctothorp = paragraph.charAt(0) == '#';
        int countOctothorp = 0;
        while (paragraph.charAt(countOctothorp) == '#'){
            countOctothorp++;
        }
        boolean isEndOctothorp = Character.isWhitespace(paragraph.charAt(countOctothorp));
        StringBuilder bordedParagraph = new StringBuilder();
        if ((countOctothorp > 0 && isEndOctothorp)) {
            bordedParagraph.append("<h").append(countOctothorp).append(">").append(paragraph.substring(countOctothorp + 1)).append("</h").append(countOctothorp).append(">");
        } else {
            bordedParagraph.append("<p>").append(paragraph).append("</p>");
        }
        /*String bordedParagraph = (isOctothorp && isEndOctothorp) ?
                "<h" + countOctothorp + ">" + paragraph.substring(countOctothorp + 1) + "</h" + countOctothorp + ">" :
                "<p>" + paragraph + "</p>";*/
        return markdownBordersToHtml(bordedParagraph.toString());
    }

    private static String markdownBordersToHtml(String paragraph) {
        Deque<StringBuilder> wordsStack = new ArrayDeque<>();
        Deque<String> marksStack = new ArrayDeque<>();
        Scanner scanner = new Scanner(paragraph);
        while (!scanner.endLine()) {
            wordsStack.push(scanner.getStringBeforeMark());
            String nowMark = scanner.nextMark();
            while (wordsStack.size() > marksStack.size() + 1) {
                StringBuilder tmp = wordsStack.pop();
                wordsStack.peek().append(tmp);
            }
            if (!marksStack.isEmpty() && marksStack.peek().contentEquals(nowMark) && BOUNDARIES.get(nowMark) != null) {
                wordsStack.push(getNewWordWithBorder(nowMark, wordsStack.pop().toString()));
                marksStack.pop();
            } else {
                marksStack.push(nowMark);
            }
        }
        marksStack.pop();
        while (wordsStack.size() != 1) {
            String tmp = wordsStack.pop().toString();
            wordsStack.peek().append(marksStack.pop()).append(tmp);
        }
        return wordsStack.pop().append("\n").toString();
    }

    private static StringBuilder getNewWordWithBorder(String border, String word) {
        StringBuilder newWordWithBorders = new StringBuilder();
        newWordWithBorders.append(BOUNDARIES.get(border)).append(word).append("</").append(BOUNDARIES.get(border).substring(1));
        return newWordWithBorders;
    }
}