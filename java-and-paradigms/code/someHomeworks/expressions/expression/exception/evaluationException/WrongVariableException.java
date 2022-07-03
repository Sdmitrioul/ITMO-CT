package expression.exception.evaluationException;

public class WrongVariableException extends EvaluationException {
    public WrongVariableException(String message) {
        super("This name of variable - " + message + ", can`t be used");
    }
}
