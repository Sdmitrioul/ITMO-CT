package generics.expression.exception.parserException;

public class BracketException extends ParserException {
    public BracketException(String message) {
        super("BracketException: " + message);
    }
}
