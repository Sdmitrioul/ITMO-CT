package expression.exception.evaluationException;

import expression.exception.ExpressionException;

public class EvaluationException extends ExpressionException {
    public EvaluationException(String message) {
        super("EvaluationException - " + message);
    }
}
