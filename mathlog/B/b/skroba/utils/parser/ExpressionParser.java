package b.skroba.utils.parser;

import b.skroba.grammar.Expression;
import b.skroba.utils.Parser;
import b.skroba.utils.source.CharSource;
import b.skroba.utils.source.StringSource;

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
