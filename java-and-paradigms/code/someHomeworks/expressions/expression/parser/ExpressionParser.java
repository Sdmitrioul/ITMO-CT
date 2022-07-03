package expression.parser;

import expression.TripleExpression;
import expression.exception.parserException.BracketException;
import expression.exception.parserException.OperandException;
import expression.exception.parserException.OperationException;
import expression.exception.parserException.ParserException;
import expression.generic.mode.AbstractMode;
import expression.operation.*;
import expression.token.Token;
import expression.token.TokenType;
import expression.token.Tokenizer;

public class ExpressionParser<T> implements Parser<T> {
    private Tokenizer tokens;
    private AbstractMode<T> mode;
    private int openeBrakets;

    public ExpressionParser(AbstractMode<T> mode) {
        this.mode = mode;
    }

    @Override
    public TripleExpression<T> parse(String expression) throws ParserException {
        tokens = new Tokenizer(expression);
        this.mode = mode;
        openeBrakets = 0;
        return fourthLevel();
    }

    private TripleExpression<T> fourthLevel() throws ParserException {
        TripleExpression<T> fourth = thirdLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();

            switch (operation.getType()) {
                case MAX:
                    fourth = new Max(fourth, thirdLevel(), mode);
                    break;

                case MIN:
                    fourth = new Min(fourth, thirdLevel(), mode);
                    break;

                default:
                    tokens.prev();
                    return fourth;
            }
        }
        return fourth;
    }

    private TripleExpression<T> thirdLevel() throws ParserException {
        TripleExpression<T> third = secondLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();

            switch (operation.getType()) {
                case PLUS:
                    third = new Add(third, secondLevel(), mode);
                    break;

                case MINUS:
                    third = new Subtract(third, secondLevel(), mode);
                    break;

                default:
                    tokens.prev();
                    return third;
            }
        }

        return third;
    }

    private TripleExpression<T> secondLevel() throws ParserException {
        TripleExpression<T> second = firstLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();
            switch (operation.getType()) {
                case MUL:
                    second = new Multiply(second, firstLevel(), mode);
                    break;

                case DIV:
                    second = new Divide(second, firstLevel(), mode);
                    break;

                default:
                    tokens.prev();
                    return second;
            }
        }
        return second;
    }

    private TripleExpression<T> firstLevel() throws ParserException {
        Token token = tokens.next();
        TripleExpression<T> firstLevel = null;

        switch (token.getType()) {
            case CONST:
                firstLevel = new Const(mode.parseNumber(token.getValue()));
                break;

            case VARIABLE:
                firstLevel = new Variable(token.getValue());
                break;

            case MINUS:
                if (tokens.hasNext() && tokens.next().getType() == TokenType.CONST) {
                    String number = "-" + tokens.current().getValue();
                    firstLevel = new Const(mode.parseNumber(number));
                } else if (tokens.past().getType() == TokenType.EMPTY && tokens.future().getType() == TokenType.END) {
                    throw new OperandException("Bare -");
                } else {
                    tokens.prev();
                    firstLevel = new Negate(firstLevel(), mode);
                }
                break;

            case COUNT:
                firstLevel = new Count(fourthLevel(), mode);
                break;

            case LEFT_BR:
                openeBrakets++;
                firstLevel = fourthLevel();
                if (tokens.past().getType() == TokenType.LEFT_BR && tokens.current().getType() != TokenType.VARIABLE)
                    throw new BracketException("(())");
                if (tokens.next().getType() != TokenType.RIGHT_BR) {
                    throw new BracketException("No closing parenthesis");
                }
                openeBrakets--;
                break;

            case RIGHT_BR:
                throw new OperandException("No last argument");

            case DIV:
            case MUL:
            case PLUS:
                if (token.getType() == TokenType.PLUS && tokens.past().getType() == TokenType.EMPTY && tokens.future().getType() == TokenType.END) {
                    throw new OperationException("Bare +");
                } else if(tokens.past().getType() == TokenType.EMPTY || tokens.past().getType() == TokenType.LEFT_BR) {
                    throw new OperandException("No first argumen");
                } else {
                    throw new OperandException("No middle argument");
                }

            case END:
                throw new OperandException("No last argument");

            case TRASH:
                if (tokens.past().getType() == TokenType.EMPTY || tokens.past().getType() == TokenType.LEFT_BR) {
                    throw new OperandException("Start symbol");
                } else if (tokens.future().getType() == TokenType.RIGHT_BR || tokens.past().getType() == TokenType.END) {
                    throw new OperandException("End symbol");
                } else {
                   throw new OperandException("Middle symbol");
                }
            case EMPTY:
                firstLevel = thirdLevel();
                break;

        }
        return firstLevel;
    }
}
