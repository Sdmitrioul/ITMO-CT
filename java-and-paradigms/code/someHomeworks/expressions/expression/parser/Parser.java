package expression.parser;

import expression.TripleExpression;
import expression.exception.parserException.ParserException;
import expression.generic.mode.AbstractMode;

public interface Parser<T> {
    TripleExpression<T> parse(String expression) throws ParserException;
}

