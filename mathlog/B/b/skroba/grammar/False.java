package b.skroba.grammar;

public class False implements Expression{
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
		return expression.getClass() == this.getClass();
	}
	
	@Override
	public int hashCode() {
		return 5;
	}
	
	@Override
	public int getPriority() {
		return 1;
	}
	
	@Override
	public String toString() {
		return "_|_";
	}
}
