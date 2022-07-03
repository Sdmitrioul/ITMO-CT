package e.skroba.grammar;

import java.util.Set;

public interface Expression {
	default boolean isNegation() {
		return false;
	}
	default boolean isImplication() {
		return false;
	}
	default boolean isDisjunction() {
		return false;
	}
	default boolean isConjunction() {
		return false;
	}
	default boolean isAll() {
		return false;
	}
	default boolean isOne() {
		return false;
	}
	default boolean isSum() {
		return false;
	}
	default boolean isMul() {
		return false;
	}
	default boolean isEquality() {
		return false;
	}
	default boolean isIncrement() {
		return false;
	}
	default boolean isVariable() {
		return false;
	}
	default boolean isPredicate() {
		return false;
	}
	default boolean isZero() {
		return false;
	}
	default boolean isEmpty() {
		return false;
	};
	Set<String> getFreeVariables();
	Expression eval(String var, int value);
	Expression diff(Variable var, Expression comparing);
	default boolean isFreeForVariables(Variable var, Set<String> free, Set<String> close) {
		return true;
	}
	boolean isNotFree(String var);
	boolean equals(Expression expression);
	int hashCode();
	int getPriority();
	String toString(boolean debug);
	String toString();
}
