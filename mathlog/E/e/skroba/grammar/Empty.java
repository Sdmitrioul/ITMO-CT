package e.skroba.grammar;

import java.util.HashSet;
import java.util.Set;

// This class should be used only for diff
public class Empty extends AbstractExpression {
	@Override
	public String toString(boolean debug) {
		return "\"EMPTY\"";
	}
	
	@Override
	public Set<String> getFreeVariables() {
		return new HashSet<>();
	}
	
	@Override
	public Expression eval(String var, int value) {
		return null;
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		return null;
	}
	
	@Override
	public boolean isEmpty() {
		return true;
	}
	
	@Override
	public boolean isNotFree(String var) {
		return true;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isEmpty();
	}
	
	@Override
	public int getPriority() {
		return 0;
	}
}
