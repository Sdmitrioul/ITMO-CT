package d.skroba.grammar.binary;

import d.skroba.grammar.Expression;
import d.skroba.grammar.LogicSign;

public final class Implication extends AbstractBinaryExpression {
	public Implication(Expression left, Expression right) {
		super(LogicSign.IMPLICATION, left, right, 5);
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Implication(left.eval(var, value), right.eval(var, value));
	}
	
	@Override
	public boolean isImplication() {
		return true;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isImplication() && expression.hashCode() == hashCode();
	}
}
