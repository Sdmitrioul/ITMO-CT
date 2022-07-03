package e.skroba.grammar.binary;

import e.skroba.grammar.Expression;
import e.skroba.grammar.LogicSign;

public final class Sum extends AbstractBinaryExpression {
	public Sum(Expression left, Expression right) {
		super(LogicSign.SUM, left, right, 4);
	}
	
	@Override
	public boolean isSum() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Sum(left.eval(var, value), right.eval(var, value));
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isSum() && expression.hashCode() == hashCode();
	}
}
