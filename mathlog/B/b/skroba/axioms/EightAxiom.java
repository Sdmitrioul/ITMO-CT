package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Disjunction;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class EightAxiom extends AbstractAxiom {
	private final Expression a;
	private final Expression b;
	private final Expression c;
	private final Implication ac;
	private final Implication bc;
	private final Disjunction disj;
	private final Implication implRight;
	private final Implication abc;
	private final Expression expression;
	
	// (A -> C) -> ((B -> C) -> ((A | B) -> C))
	public EightAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
		}
		
		this.expression = expression;
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication() || !impl.first.isImplication()) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
		}
		
		ac = (Implication) impl.first;
		a = ac.first;
		c = ac.second;
		implRight = (Implication) impl.second;
		
		if (!implRight.first.isImplication() || !implRight.second.isImplication()) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
		}
		
		bc = (Implication) implRight.first;
		b = bc.first;
		if (!bc.second.equals(c)) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
		}
		
		if (!implRight.second.isImplication()) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
		}
		
		abc = (Implication) implRight.second;
		
		if (!abc.first.isDisjunction() || !abc.second.equals(c)) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
		}
		
		disj = (Disjunction) abc.first;
		
		if (!disj.first.equals(a) || !disj.second.equals(b)) {
			throw new AxiomException("Expected Eight Axiom <(A -> C) -> ((B -> C) -> ((A | B) -> C))>, got: " + expression);
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
		final Expression c = implLeft.second;
		
		final Implication implRight = (Implication) impl.second;
		
		if (!implRight.first.isImplication() || !implRight.second.isImplication()) {
			return false;
		}
		
		final Implication implRightLeft = (Implication) implRight.first;
		final Expression b = implRightLeft.first;
		if (!implRightLeft.second.equals(c)) {
			return false;
		}
		
		return implRight.second.equals(new Implication(new Disjunction(a, b), c));
	}
	
	
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, ac, pos + 5, "Ax", ac, bc, disj, a));
		sb.append(line(list, a, pos + 5, "Ax", ac, bc, disj, a));
		sb.append(line(list, c, pos + 4, "E->", ac, bc, disj, a));
		
		sb.append(line(list, bc, pos + 5, "Ax", ac, bc, disj, b));
		sb.append(line(list, b, pos + 5, "Ax", ac, bc, disj, b));
		sb.append(line(list, c, pos + 4, "E->", ac, bc, disj, b));
		
		sb.append(line(list, disj, pos + 4, "Ax", ac, bc, disj));
		sb.append(line(list, c, pos + 3, "E|", ac, bc, disj));
		sb.append(line(list, abc, pos + 2, "I->", ac, bc));
		sb.append(line(list, implRight, pos + 1, "I->", ac));
		sb.append(line(list, expression, pos, "I->"));
		return sb.toString();
	}
}
