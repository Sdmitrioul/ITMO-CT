package expression;

import expression.exceptions.ExpressionParser;
import expression.exceptions.ExpressionParsingException;
import expression.token.Tokenizer;

public class Main {
    public static void main(String[] args) throws ExpressionParsingException {
        Tokenizer tokenizer = new Tokenizer("x*y+(z-1   )/10");
        ExpressionParser expressionParser = new ExpressionParser();
        TripleExpression tripleExpression = expressionParser.parse("x*y+(z-1   )/10");
        System.out.println( tripleExpression.toString());
    }
}
