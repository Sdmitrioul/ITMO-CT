import pushingNegatives.Expression;
import pushingNegatives.parser.ExpressionParser;
import pushingNegatives.parser.Parser;

public class Main {
    public static void main(String[] args) {
        String string = "~(1 & ~a) | (b | c) & d";//"~(a & b) & ~(~~c | ~1)";
        //String string = args[0];
        Parser parser = new ExpressionParser();
        Expression expression = parser.parse(string);
        System.out.println(expression.toString());
    }
}
