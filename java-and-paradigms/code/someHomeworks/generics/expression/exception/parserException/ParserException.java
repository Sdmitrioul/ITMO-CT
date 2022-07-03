package generics.expression.exception.parserException;

import generics.expression.exception.ExpressionException;

public class ParserException extends ExpressionException {
    public ParserException(String message) {
        super("ParsingException - " + message);
    }
}
