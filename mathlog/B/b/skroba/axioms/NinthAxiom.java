package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.False;
import b.skroba.grammar.binary.Implication;
import b.skroba.grammar.unary.Negation;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class NinthAxiom extends AbstractAxiom {
	private final Implication ab;
	private final Expression a;
	private final Expression b;
	private final Expression neg = new False();
	
	// (A -> B) -> ((A -> !B) -> !A)
	public NinthAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Ninth Axiom <(A -> B) -> ((A -> !B) -> !A)>, got: " + expression);
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication() || !impl.first.isImplication()) {
			throw new AxiomException("Expected Ninth Axiom <(A -> B) -> ((A -> !B) -> !A)>, got: " + expression);
		}
		
		ab = (Implication) impl.first;
		a = ab.first;
		b = ab.second;
		
		if (!impl.second.equals(new Implication(new Implication(a, new Negation(b)), new Negation(a)))) {
			throw new AxiomException("Expected Ninth Axiom <(A -> B) -> ((A -> !B) -> !A)>, got: " + expression);
		}
	}
	
	public static boolean isAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication() || !impl.first.isImplication()) {
			return false;
		}
		
		final Implication implLeft = (Implication) impl.first;
		final Expression a = implLeft.first;
		final Expression b = implLeft.second;
		
		final Implication implRight = (Implication) impl.second;
		
		if (!implRight.first.isImplication() || !implRight.second.isNegation()) {
			return false;
		}
		
		return implRight.first.equals(new Implication(a, new Negation(b))) && implRight.second.equals(new Negation(a));
	}
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		final Expression bN = new Implication(b, neg);
		final Expression abN = new Implication(a, bN);
		final Expression aN = new Implication(a, neg);
		final Expression abNaN = new Implication(abN, aN);
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, abN, pos + 5, "Ax", ab, abN, a));
		sb.append(line(list, a, pos + 5, "Ax", ab, abN, a));
		sb.append(line(list, bN, pos + 4, "E->", ab, abN, a));
		sb.append(line(list, ab, pos + 5, "Ax", ab, abN, a));
		sb.append(line(list, a, pos + 5, "Ax", ab, abN, a));
		sb.append(line(list, b, pos + 4, "E->", ab, abN, a));
		sb.append(line(list, neg, pos + 3, "E->", ab, abN, a));
		sb.append(line(list, aN, pos + 2, "I->", ab, abN));
		sb.append(line(list, abNaN, pos + 1, "I->", ab));
		sb.append(line(list, new Implication(ab, abNaN), pos, "I->"));
		return sb.toString();
	}
}
