package generics.expression.exception.parserException;

public class OperandException extends ParserException {
    public OperandException(String message) {
        super("OperandException: " + message);
    }
}
