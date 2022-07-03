package knf.parser;

import knf.Expression;

import java.util.Map;

public interface Parser {
    Expression parse(String expression);
    Map<Integer, Character> getVariables();
}
