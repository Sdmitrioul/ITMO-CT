package generics.expression.generic;

import generics.expression.TripleExpression;
import generics.expression.exception.ExpressionException;
import generics.expression.exception.evaluationException.EvaluationException;
import generics.expression.exception.parserException.ParserException;
import generics.expression.generic.mode.*;
import generics.expression.parser.ExpressionParser;

public class GenericTabulator implements Tabulator {
    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        AbstractMode<?> abstractMode;
        switch (mode) {
            case "i":
                abstractMode = new IntegerMode();
                break;
            case "d":
                abstractMode = new DoubleMode();
                break;
            case "bi":
                abstractMode = new BigIntegerMode();
                break;
            case "u":
                abstractMode = new IntMode();
                break;
            case "l":
                abstractMode = new LongMode();
                break;
            case "s":
                abstractMode = new ShortMode();
                break;
            default:
                throw new ExpressionException(mode);
        }
        return fillTable(abstractMode, expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] fillTable(AbstractMode<T> mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        Object[][][] res = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        TripleExpression<T> parsedExpression;
        try {
            parsedExpression = new ExpressionParser<>(mode).parse(expression);
        } catch (ParserException e) {
            return res;
        }
        for (int ix = x1; ix <= x2; ix++) {
            for (int iy = y1; iy <= y2; iy++) {
                for (int iz = z1; iz <= z2; iz++) {
                    try {
                        res[ix - x1][iy - y1][iz - z1] = parsedExpression.evaluate(
                                mode.parseNumber(Integer.toString(ix)),
                                mode.parseNumber(Integer.toString(iy)),
                                mode.parseNumber(Integer.toString(iz)));
                    } catch (EvaluationException e) {
                        res[ix - x1][iy - y1][iz - z1] = null;
                    }
                }
            }
        }
        return res;
    }
}
