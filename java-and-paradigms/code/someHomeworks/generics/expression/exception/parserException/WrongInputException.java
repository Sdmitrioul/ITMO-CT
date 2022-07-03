package generics.expression.exception.parserException;

public class WrongInputException extends ParserException {
    public WrongInputException(String message) {
        super("Wrong input: " + message);
    }
}
