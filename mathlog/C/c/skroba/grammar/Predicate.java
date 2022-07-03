package c.skroba.grammar;

import java.util.HashSet;
import java.util.Set;

public final class Predicate extends AbstractExpression {
	private final String name;
	private int hash;
	private boolean calculated;
	
	public Predicate(String name) {
		this.name = name;
	}
	
	public Predicate(char ch) {
		this.name = String.valueOf(ch);
	}
	
	@Override
	public String toString(boolean debug) {
		return name;
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
	public boolean isNotFree(String var) {
		return true;
	}
	
	@Override
	public boolean isPredicate() {
		return true;
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		return comparing.isPredicate() && comparing.equals(this) ? new Empty() : null;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isPredicate() && expression.hashCode() == hashCode();
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			calculated = true;
			hash = name.hashCode();
		}
		return hash;
	}
	
	@Override
	public int getPriority() {
		return 1;
	}
}
