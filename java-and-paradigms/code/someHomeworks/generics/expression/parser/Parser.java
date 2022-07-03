package generics.expression.parser;

import generics.expression.TripleExpression;
import generics.expression.exception.parserException.ParserException;

public interface Parser<T> {
    TripleExpression<T> parse(String expression) throws ParserException;
}

