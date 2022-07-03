package a.skroba.utils.parser;

import a.skroba.grammar.Expression;
import a.skroba.utils.Parser;
import a.skroba.utils.source.CharSource;
import a.skroba.utils.source.StringSource;

public final class ExpressionParser implements Parser<Expression> {
	public static Expression parse(final String source) {
		return new LogicExpressionParser(new StringSource(source)).parse();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public Expression parse(final CharSource source) {
		return new LogicExpressionParser(source).parse();
	}
}
