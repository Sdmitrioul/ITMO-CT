package expression.exception.parserException;

public class NumberFormatException extends ParserException {
    public NumberFormatException(String message) {
        super("NumberFormatException" + message);
    }
}
