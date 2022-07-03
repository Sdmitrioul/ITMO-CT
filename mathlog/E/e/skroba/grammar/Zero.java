package e.skroba.grammar;

import java.util.HashSet;
import java.util.Set;

public final class Zero extends AbstractExpression {
	@Override
	public String toString(boolean debug) {
		return "0";
	}
	
	@Override
	public Set<String> getFreeVariables() {
		return new HashSet<>();
	}
	
	@Override
	public Expression eval(String var, int value) {
		return this;
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		return comparing.isZero() ? new Empty() : null;
	}
	
	@Override
	public boolean isZero() {
		return true;
	}
	
	@Override
	public boolean isNotFree(String var) {
		return true;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isZero();
	}
	
	@Override
	public int hashCode() {
		return 199;
	}
	
	@Override
	public int getPriority() {
		return 1;
	}
}
