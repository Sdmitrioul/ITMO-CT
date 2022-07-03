package b.skroba;

import b.skroba.axioms.*;
import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.Pair;
import b.skroba.utils.exception.AxiomException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Checker {
	public static List<Axioms> check(Pair<Proofing, List<Expression>> inputData) {
		final List<Expression> proofed = new ArrayList<>();
		final HashMap<Integer, Integer> proofedHash = new HashMap<>();
		final List<Axioms> rules = new ArrayList<>();
		for (int i = 0; i < inputData.second.size(); i++) {
			final Expression expression = inputData.second.get(i);
			Axioms axiom = checkOnAxiomOrHypothesis(expression, inputData.first.hypothesis);
			
			if (axiom == null) {
				axiom = checkOnMP(expression, proofed, proofedHash);
			}
			
			rules.add(axiom);
			proofed.add(expression);
			proofedHash.put(expression.hashCode(), i);
		}
		
		if (!inputData.first.expression.equals(inputData.second.get(inputData.second.size() - 1))) {
			throw new AxiomException("The proof proves different expression.");
		}
		
		return rules;
	}
	
	private static Axioms checkOnAxiomOrHypothesis(final Expression expression, final List<Expression> hypothesis) {
		if (FirstAxiom.isAxiom(expression)) {
			return new FirstAxiom(expression);
		} else if (SecondAxiom.isAxiom(expression)) {
			return new SecondAxiom(expression);
		} else if (ThirdAxiom.isAxiom(expression)) {
			return new ThirdAxiom(expression);
		} else if (FourthAndFifthAxiom.isAxiom(expression)) {
			return new FourthAndFifthAxiom(expression);
		} else if (SixAndSevenAxiom.isAxiom(expression)) {
			return new SixAndSevenAxiom(expression);
		} else if (EightAxiom.isAxiom(expression)) {
			return new EightAxiom(expression);
		} else if (NinthAxiom.isAxiom(expression)) {
			return new NinthAxiom(expression);
		} else if (TenAxiom.isAxiom(expression)) {
			return new TenAxiom(expression);
		}
		
		return hypothesis.stream().anyMatch(x -> x.equals(expression)) ? new Axiom(expression) : null;
	}
	
	private static Axioms checkOnMP(final Expression expression, final List<Expression> proofed, final HashMap<Integer, Integer> proofedHash) {
		for (int i = proofed.size() - 1; i >= 0; i--) {
			if (!proofed.get(i).isImplication()) {
				continue;
			}
			
			final Implication impl = (Implication) proofed.get(i);
			
			if (!expression.equals(impl.second)) {
				continue;
			}
			
			final Integer j = proofedHash.get(impl.first.hashCode());
			if (j != null) {
				return new ModusPonuns(expression, i, j);
			}
		}
		
		throw new AxiomException("Proof is incorrect at line " + (proofed.size() + 2));
	}
}
