package e.skroba.grammar.calculus;

import e.skroba.grammar.Empty;
import e.skroba.grammar.Expression;
import e.skroba.grammar.Variable;
import e.skroba.grammar.LogicSign;

public final class Exist extends AbstractCalculusExpression {
	public Exist(Variable variable, Expression expression) {
		super(LogicSign.EXIST, variable, expression, 6);
	}
	
	@Override
	public boolean isOne() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return var.equals(variable.name) ? expression.eval(var, value) : new Exist(variable, expression.eval(var, value));
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		if (var.equals(variable) && comparing.equals(this)) {
			return new Empty();
		}
		
		if (!comparing.isOne()) {
			return null;
		}
		
		final Exist exist = (Exist) comparing;
		return exist.variable.equals(variable) && !variable.equals(var) ? expression.diff(var, exist.expression) : null;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isOne() && expression.hashCode() == hashCode();
	}
}
