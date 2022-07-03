package b.skroba.grammar;

public interface Expression {
	boolean isNegation();
	boolean isImplication();
	boolean isDisjunction();
	boolean isConjunction();
	boolean equals(Expression expression);
	int hashCode();
	int getPriority();
	String toString();
}
