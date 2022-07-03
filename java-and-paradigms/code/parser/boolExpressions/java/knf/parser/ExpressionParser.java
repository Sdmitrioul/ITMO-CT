package knf.parser;

import knf.*;
import knf.token.*;

import java.util.Map;

public class ExpressionParser implements Parser {
    private Tokenizer tokens;

    @Override
    public Expression parse(String expression) {
        tokens = new Tokenizer(expression);
        return thirdLevel();
    }

    public Map<Integer, Character> getVariables() {
        return tokens.getVariables();
    }

    private Expression thirdLevel() {
        Expression third = secondLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();

            switch (operation.getType()) {
                case DISJUNCTION:
                    third = new Disjunction(third, secondLevel());
                    break;

                default:
                    tokens.prev();
                    return third;
            }
        }

        return third;
    }

    private Expression secondLevel() {
        Expression second = firstLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();

            switch (operation.getType()) {
                case CONJUNCTION:
                    second = new Conjunction(second, firstLevel());
                    break;
                default:
                    tokens.prev();
                    return second;
            }
        }

        return second;
    }

    private Expression firstLevel() {
        Expression first = null;
        Token token = tokens.next();

        switch (token.getType()) {
            case TRUE:
                first = new Const(true);
                break;
            case FALSE:
                first = new Const(false);
                break;
            case VARIABLE:
                first = new Variable(token.getValue());
                break;
            case NEGATION:
                if (tokens.hasNext() && tokens.next().getType() == TokenType.TRUE) {
                    first = new Const(false);
                } else {
                    tokens.prev();
                    if (tokens.hasNext() && tokens.next().getType() == TokenType.FALSE) {
                        first = new Const(true);
                    } else {
                        tokens.prev();
                        first = new Negation(firstLevel());
                    }
                }
                break;
            case LEFT_BR:
                first = thirdLevel();
                tokens.next();
                break;
        }
        return first;
    }
}
