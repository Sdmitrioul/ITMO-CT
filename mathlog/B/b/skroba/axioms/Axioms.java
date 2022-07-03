package b.skroba.axioms;

import b.skroba.grammar.Expression;

import java.util.List;

public interface Axioms {
	String toNaturalOutput(List<Expression> list, int pos);
	static boolean isAxiom(Expression expression) {
		return false;
	}
	String line(List<Expression> list, Expression expression, int pos, String move, Expression... hypothesis);
	default boolean isMP() {
		return false;
	}
}
