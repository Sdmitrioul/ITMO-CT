package expression.exceptions;

import expression.*;
import expression.token.Token;
import expression.token.TokenType;
import expression.token.Tokenizer;


public class ExpressionParser implements Parser {
    private Tokenizer tokens;
    private int openeBrakets;

    public TripleExpression parse(String expression) throws ExpressionParsingException {
        tokens = new Tokenizer(expression);
        openeBrakets = 0;
        return thirdLevel();
    }

    private CommonExpression thirdLevel() throws ExpressionParsingException {
        CommonExpression third = secondLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();

            switch (operation.getType()) {
                case PLUS:
                    third = new CheckedAdd(third, secondLevel());
                    break;

                case MINUS:
                    third = new CheckedSubtract(third, secondLevel());
                    break;

                /*case RIGHT_BR:
                    throw new ExpressionParsingException("No opening parenthesi");*/

                case CONST:
                    throw new ExpressionParsingException("Spaces in numbers");

                case LEFT_BR:
                    throw new ExpressionParsingException("End symbol");

                case TRASH:
                    throw new ExpressionParsingException("End symbol");

                case LOG:
                    throw new ExpressionParsingException("Parse problem");

                case POW:
                    throw new ExpressionParsingException("Parse problem");


                default:
                    tokens.prev();
                    return third;
            }
        }

        return third;
    }

    private CommonExpression secondLevel() throws ExpressionParsingException {
        CommonExpression second = firstLevel();

        while (tokens.hasNext()) {
            Token operation = tokens.next();
            switch (operation.getType()) {
                case MUL:
                    second = new CheckedMultiply(second, firstLevel());
                    break;

                case DIV:
                    second = new CheckedDivide(second, firstLevel());
                    break;

                case RIGHT_BR:
                    if (openeBrakets == 0) {
                        throw new ExpressionParsingException("No opening parenthesi");
                    } else {
                        tokens.prev();
                        return second;
                    }

                case CONST:
                    throw new ExpressionParsingException("Spaces in numbers");

                case LEFT_BR:
                    throw new ExpressionParsingException("End symbol");

                case TRASH:
                    throw new ExpressionParsingException("End symbol");

                case LOG:
                    throw new ExpressionParsingException("Parse problem");

                case POW:
                    throw new ExpressionParsingException("Parse problem");

                default:
                    tokens.prev();
                    return second;
            }
        }
        return second;
    }

    private CommonExpression firstLevel() throws ExpressionParsingException {
        Token token = tokens.next();
        CommonExpression firstLevel = null;

        switch (token.getType()) {
            case CONST:
                firstLevel = new Const(Integer.parseInt(token.getValue()));
                break;

            case VARIABLE:
                firstLevel = new Variable(token.getValue());
                break;

            case MINUS:
                if (tokens.hasNext() && tokens.next().getType() == TokenType.CONST) {
                    String number = "-" + tokens.current().getValue();
                    firstLevel = new Const(Integer.parseInt(number));
                } else if (tokens.past().getType() == TokenType.EMPTY && tokens.future().getType() == TokenType.END) {
                    throw new ExpressionParsingException("Bare -");
                } else {
                    tokens.prev();
                    firstLevel = new CheckedNegate(firstLevel());
                }
                break;

            case POW:
                firstLevel = new Pow2(firstLevel());
                break;

            case LOG:
                firstLevel = new Log2(firstLevel());
                break;

            case LEFT_BR:
                openeBrakets++;
                firstLevel = thirdLevel();
                /*if (tokens.past().getType() == TokenType.LEFT_BR && tokens.current().getType() != TokenType.VARIABLE)
                    throw new ExpressionParsingException("(())");*/
                if (tokens.next().getType() != TokenType.RIGHT_BR) {
                    throw new ExpressionParsingException("No closing parenthesis");
                }
                openeBrakets--;
                break;

            case RIGHT_BR:
                if (tokens.past().getType() == TokenType.CONST || tokens.past().getType() == TokenType.VARIABLE) {
                    throw new ExpressionParsingException("No last argument");
                }
                throw new ExpressionParsingException("No last argument");

            case DIV:
            case MUL:
            case PLUS:
                if (token.getType() == TokenType.PLUS && tokens.past().getType() == TokenType.EMPTY && tokens.future().getType() == TokenType.END) {
                    throw new ExpressionParsingException("Bare +");
                } else if(tokens.past().getType() == TokenType.EMPTY || tokens.past().getType() == TokenType.LEFT_BR) {
                    throw new ExpressionParsingException("No first argumen");
                } else {
                    throw new ExpressionParsingException("No middle argument");
                }

            case END:
                throw new ExpressionParsingException("No last argument");

            case TRASH:
                if (tokens.past().getType() == TokenType.EMPTY || tokens.past().getType() == TokenType.LEFT_BR) {
                    throw new ExpressionParsingException("Start symbol");
                } else if (tokens.future().getType() == TokenType.RIGHT_BR || tokens.past().getType() == TokenType.END) {
                    throw new ExpressionParsingException("End symbol");
                } else {
                    throw new ExpressionParsingException("Middle symbol");
                }
            case EMPTY:
                firstLevel = thirdLevel();
                break;
        }
        return firstLevel;
    }
}
