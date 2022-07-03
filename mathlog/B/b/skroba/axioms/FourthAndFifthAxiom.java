package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Conjunction;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class FourthAndFifthAxiom extends AbstractAxiom {
	private Expression c;
	private Conjunction conj;
	private Expression expression;
	
	// (A & B) -> A or (A & B) -> B
	public FourthAndFifthAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Fourth or Fifth Axiom <(A | B) -> A or (A | B) -> B>, got: " + expression);
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.first.isConjunction()) {
			throw new AxiomException("Expected Fourth or Fifth Axiom <(A | B) -> A or (A | B) -> B>, got: " + expression);
		}
		
		final Conjunction conj = (Conjunction) impl.first;
		
		if (!conj.first.equals(impl.second) && !conj.second.equals(impl.second)) {
			throw new AxiomException("Expected Fourth or Fifth Axiom <(A | B) -> A or (A | B) -> B>, got: " + expression);
		}
		
		this.expression = expression;
		this.conj = conj;
		c = impl.second;
	}
	
	public static boolean isAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.first.isConjunction()) {
			return false;
		}
		
		final Conjunction conj = (Conjunction) impl.first;
		
		if (!conj.first.equals(impl.second) && !conj.second.equals(impl.second)) {
			return false;
		}
		
		return true;
	}
	
	private String commonNaturalForm(final List<Expression> list, final int pos, final String position) {
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, conj, pos + 2, "Ax", conj));
		sb.append(line(list, c, pos + 1, position, conj));
		sb.append(line(list, expression, pos, "I->"));
		return sb.toString();
	}
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		return conj.first.equals(c) ?
				commonNaturalForm(list, pos, "El&") :
				commonNaturalForm(list, pos, "Er&");
	}
}
