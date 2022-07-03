package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Disjunction;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class SixAndSevenAxiom extends AbstractAxiom {
	private Expression c;
	private Disjunction disj;
	private Expression expression;
	
	// A -> (A | B) or B -> (A | B)
	public SixAndSevenAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Six or Seven Axiom <A -> (A | B) or B -> (A | B)>, got: " + expression);
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isDisjunction()) {
			throw new AxiomException("Expected Six or Seven Axiom <A -> (A | B) or B -> (A | B)>, got: " + expression);
		}
		
		final Disjunction disj = (Disjunction) impl.second;
		
		if (!disj.first.equals(impl.first) && !disj.second.equals(impl.first)) {
			throw new AxiomException("Expected Six or Seven Axiom <A -> (A | B) or B -> (A | B)>, got: " + expression);
		}
		
		c = impl.first;
		this.expression = expression;
		this.disj = disj;
	}
	
	public static boolean isAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.second.isDisjunction()) {
			return false;
		}
		
		final Disjunction disj = (Disjunction) impl.second;
		
		if (!disj.first.equals(impl.first) && !disj.second.equals(impl.first)) {
			return false;
		}
		
		return true;
	}
	
	private String commonNaturalForm(final List<Expression> list, final int pos, final String position) {
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, c, pos + 2, "Ax", c));
		sb.append(line(list, disj, pos + 1, position, c));
		sb.append(line(list, expression, pos, "I->"));
		return sb.toString();
	}
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		return disj.first.equals(c) ?
				commonNaturalForm(list, pos, "Il|") :
				commonNaturalForm(list, pos, "Ir|");
	}
}
