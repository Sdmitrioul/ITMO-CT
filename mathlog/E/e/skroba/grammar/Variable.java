package e.skroba.grammar;

import e.skroba.grammar.unary.Number;

import java.util.HashSet;
import java.util.Set;

public final class Variable extends AbstractExpression {
	public final String name;
	private int hash;
	private boolean calculated;
	
	public Variable(final String name) {
		this.name = name;
	}
	
	public Variable(char ch) {
		this.name = String.valueOf(ch);
	}
	
	@Override
	public int getPriority() {
		return 1;
	}
	
	@Override
	public String toString(boolean debug) {
		return name;
	}
	
	@Override
	public Set<String> getFreeVariables() {
		final Set<String> res = new HashSet<>();
		res.add(name);
		return res;
	}
	
	@Override
	public Expression eval(String var, int value) {
			return var.equals(name) ? Number.getNumber(value) : this;
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		if (var.equals(this)) {
			return comparing;
		}
		return comparing.isVariable() && comparing.equals(this) ? new Empty() : null;
	}
	
	@Override
	public boolean isFreeForVariables(Variable var, Set<String> free, Set<String> close) {
		if (!var.equals(this)) {
			return true;
		}
		
		return close.stream().noneMatch(free::contains);
	}
	
	@Override
	public boolean isNotFree(String var) {
		return !var.equals(name);
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isVariable() && expression.hashCode() == hashCode();
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			this.hash = name.hashCode();
			calculated = true;
		}
		return this.hash;
	}
	
	@Override
	public boolean isVariable() {
		return true;
	}
}
