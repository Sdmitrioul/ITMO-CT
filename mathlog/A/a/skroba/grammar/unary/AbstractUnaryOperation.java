package a.skroba.grammar.unary;

import a.skroba.grammar.Expression;
import a.skroba.grammar.LogicSign;

public abstract class AbstractUnaryOperation implements Expression {
	public final LogicSign sign;
	public final Expression expression;
	
	public AbstractUnaryOperation(final LogicSign sign, final Expression expression) {
		this.sign = sign;
		this.expression = expression;
	}
	
	@Override
	public String toSimpleTreeGrammar() {
		return "(" + sign
				+ expression.toSimpleTreeGrammar()
				+ ")";
	}
}
