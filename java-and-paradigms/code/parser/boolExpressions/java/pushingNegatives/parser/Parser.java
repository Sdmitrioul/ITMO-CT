package pushingNegatives.parser;

import pushingNegatives.Expression;

import java.util.Map;

public interface Parser {
    Expression parse(String expression);
}
