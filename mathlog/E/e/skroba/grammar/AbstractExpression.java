package e.skroba.grammar;

public abstract class AbstractExpression implements Expression {
	public abstract String toString(boolean debug);
	
	@Override
	public String toString() {
		return toString(false);
	}
}
