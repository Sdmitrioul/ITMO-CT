import java.util.HashMap;
import java.util.Map;

public class RomanNumerals {
    private static final Map<Character, Integer> FROM_ROMAN =  Map.of(
            'I', 1,
            'V', 5,
            'X', 10,
            'L', 50,
            'C', 100,
            'D', 500,
            'M', 1000
    );

    public static void main(String[] args) {
        System.out.println(stripComments("apples, pears # and bananas\ngrapes\nbananas !apples", new String[] {"#", "!"}));
        //System.out.println(makeReadable(5));
    }

    public static String stripComments(String text, String[] commentSymbols) {
        String[] paragraphs = text.split("\n");
        StringBuilder ans = new StringBuilder();
        for (String paragraph : paragraphs) {
            for (String com : commentSymbols) {
                System.out.println(paragraph.indexOf(com));
                if (paragraph.indexOf(com) != -1) {
                    paragraph =  paragraph.substring(0, paragraph.indexOf(com));
                }
            }
            int i = paragraph.length();
            while (i != 0 && Character.isWhitespace(paragraph.charAt(i - 1))) {
                i--;
            }
        }
        ans.deleteCharAt(ans.length() - 1);
        return ans.toString();
    }

    public static String makeReadable(int seconds) {
        int minutes = seconds / 60;
        seconds %= 60;
        int hours = minutes / 60;
        minutes %= 60;

        return String.format("%02d:%02d:%02d", hours, minutes, seconds);
    }

    public static int fromRoman(String romanNumeral) {
        return 1;
    }
}
