package d.skroba.grammar.binary;

import d.skroba.grammar.AbstractExpression;
import d.skroba.grammar.Expression;
import d.skroba.grammar.LogicSign;
import d.skroba.grammar.Variable;

import java.util.Set;

public abstract class AbstractBinaryExpression extends AbstractExpression {
	private final LogicSign sign;
	public final Expression left;
	public final Expression right;
	private final int priority;
	private boolean calculated;
	private int hash;
	
	public AbstractBinaryExpression(LogicSign sign, Expression left, Expression right, int priority) {
		this.sign = sign;
		this.left = left;
		this.right = right;
		this.priority = priority;
	}
	
	@Override
	public String toString(boolean debug) {
		return "(" + left + sign + right +")";
	}
	
	@Override
	public Set<String> getFreeVariables() {
		Set<String> freeVariables = left.getFreeVariables();
		freeVariables.addAll(right.getFreeVariables());
		return freeVariables;
	}
	
	@Override
	public boolean isNotFree(String var) {
		return left.isNotFree(var) && right.isNotFree(var);
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		if (comparing.getClass() != this.getClass()) {
			return null;
		}
		
		final AbstractBinaryExpression bin = (AbstractBinaryExpression) comparing;
		
		final Expression left = this.left.diff(var, bin.left);
		final Expression right = this.right.diff(var, bin.right);
		
		if (left == null || right == null) {
			return null;
		}
		
		if (left.isEmpty() && right.isEmpty()) {
			return left;
		}
		
		if (left.isEmpty() || right.isEmpty()) {
			return left.isEmpty() ? right : left;
		}
		
		return left.equals(right) ? left : null;
	}
	
	@Override
	public boolean isFreeForVariables(Variable var, Set<String> free, Set<String> close) {
		return left.isFreeForVariables(var, free, close) && right.isFreeForVariables(var, free, close);
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			int result = left.hashCode() * 23;
			result += right.hashCode();
			result *= 23;
			result += sign.name().hashCode();
			calculated = true;
			hash = result;
		}
		return hash;
	}
	
	@Override
	public int getPriority() {
		return priority;
	}
}
