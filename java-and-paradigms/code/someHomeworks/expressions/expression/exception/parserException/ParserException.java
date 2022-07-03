package expression.exception.parserException;

import expression.exception.ExpressionException;

public class ParserException extends ExpressionException {
    public ParserException(String message) {
        super("ParsingException - " + message);
    }
}
