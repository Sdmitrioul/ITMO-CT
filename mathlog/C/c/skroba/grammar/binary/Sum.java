package c.skroba.grammar.binary;

import c.skroba.grammar.Expression;
import c.skroba.grammar.Variable;

import static c.skroba.grammar.LogicSign.SUM;

public final class Sum extends AbstractBinaryExpression {
	public Sum(Expression left, Expression right) {
		super(SUM, left, right, 4);
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
