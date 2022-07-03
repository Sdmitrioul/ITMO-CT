package generics.expression.exception.parserException;

public class OperationException extends ParserException {
    public OperationException(String message) {
        super("OperationException: " + message);
    }
}
