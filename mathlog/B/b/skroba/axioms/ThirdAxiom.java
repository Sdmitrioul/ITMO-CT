package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Conjunction;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class ThirdAxiom extends AbstractAxiom {
	private Expression a;
	private Expression b;
	private Expression conj;
	private Implication expression;
	
	// A -> (B -> (A & B))
	public ThirdAxiom(Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Third Axiom <A -> (B -> (A & B))>, got: " + expression);
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication()) {
			throw new AxiomException("Expected Third Axiom <A -> (B -> (A & B))>, got: " + expression);
		}
		
		final Implication implRight = (Implication) impl.second;
		
		if (!implRight.second.isConjunction()) {
			throw new AxiomException("Expected Third Axiom <A -> (B -> (A & B))>, got: " + expression);
		}
		
		a = impl.first;
		b = implRight.first;
		
		if (!implRight.second.equals(new Conjunction(a, b))) {
			throw new AxiomException("Not eq - Expected Third Axiom <A -> (B -> (A & B))>, got: " + expression);
		}
		
		this.expression = impl;
		conj = implRight.second;
	}
	// A -> (B -> (A & B))
	public static boolean isAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication()) {
			return false;
		}
		
		final Implication implRight = (Implication) impl.second;
		
		if (!implRight.second.isConjunction()) {
			return false;
		}
		
		final Expression a = impl.first;
		final Expression b = implRight.first;
		
		return implRight.second.equals(new Conjunction(a, b));
	}
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, a, pos + 3, "Ax", b, a));
		sb.append(line(list, b, pos + 3, "Ax", a, b));
		sb.append(line(list, conj, pos + 2, "I&", a, b));
		sb.append(line(list, expression.second, pos + 1, "I->", a));
		sb.append(line(list, expression, pos, "I->"));
		return sb.toString();
	}
}
