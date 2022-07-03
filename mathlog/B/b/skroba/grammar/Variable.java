package b.skroba.grammar;

public final class Variable implements Expression {
	private final int PRIORITY = 1;
	public final String name;
	private int hash = 0;
	private boolean calculated = false;
	
	public Variable(final String name) {
		this.name = name;
	}
	
	@Override
	public boolean isNegation() {
		return false;
	}
	
	@Override
	public boolean isImplication() {
		return false;
	}
	
	@Override
	public boolean isDisjunction() {
		return false;
	}
	
	@Override
	public boolean isConjunction() {
		return false;
	}
	
	@Override
	public boolean equals(Expression expression) {
		if (expression.isNegation() || expression.isConjunction() || expression.isDisjunction() || expression.isImplication()) {
			return false;
		}
		return expression.hashCode() == hashCode() && expression.getClass() == this.getClass();
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
		return PRIORITY;
	}
	
	@Override
	public String toString() {
		return name;
	}
}
