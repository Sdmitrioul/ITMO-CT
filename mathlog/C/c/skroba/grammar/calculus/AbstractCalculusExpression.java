package c.skroba.grammar.calculus;

import c.skroba.grammar.AbstractExpression;
import c.skroba.grammar.Expression;
import c.skroba.grammar.LogicSign;
import c.skroba.grammar.Variable;

import java.util.Set;

public abstract class AbstractCalculusExpression extends AbstractExpression {
	private final LogicSign sign;
	public final Variable variable;
	public final Expression expression;
	private final int priority;
	private int hash;
	private boolean calculated;
	
	public AbstractCalculusExpression(LogicSign sign, Variable variable, Expression expression, int priority) {
		this.sign = sign;
		this.variable = variable;
		this.expression = expression;
		this.priority = priority;
	}
	
	@Override
	public String toString(boolean debug) {
		return "(" + sign.toString() + variable + "." + expression.toString(debug) + ")";
	}
	
	@Override
	public int getPriority() {
		return priority;
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			int result = sign.name().hashCode() * 23;
			result += variable.hashCode();
			result *= 23;
			result += ".".hashCode();
			result *= 23;
			result += expression.hashCode();
			calculated = true;
			hash = result;
		}
		return hash;
	}
	
	@Override
	public Set<String> getFreeVariables() {
		Set<String> free  = expression.getFreeVariables();
		free.remove(variable.name);
		return free;
	}
	
	@Override
	public boolean isFreeForVariables(Variable var, Set<String> free, Set<String> close) {
		if (var.equals(variable)) {
			return expression.isFreeForVariables(var, free, close);
		}
		
		close.add(variable.name);
		final boolean result = expression.isFreeForVariables(var, free, close);
		close.remove(variable.name);
		return result;
	}
	
	@Override
	public boolean isNotFree(String var) {
		return !variable.isNotFree(var) || expression.isNotFree(var);
	}
}
