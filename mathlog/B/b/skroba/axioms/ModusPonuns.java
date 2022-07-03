package b.skroba.axioms;

import b.skroba.grammar.Expression;

import java.util.List;

public class ModusPonuns extends AbstractAxiom {
	private final Expression expression;
	public final int leftRule;
	public final int rightRule;
	
	public ModusPonuns(Expression expression, int leftRule, int rightRule) {
		this.expression = expression;
		this.leftRule = leftRule;
		this.rightRule = rightRule;
	}
	
	@Override
	public String toNaturalOutput(List<Expression> list, int pos) {
		return line(list, expression, pos, "E->");
	}
	
	@Override
	public boolean isMP() {
		return true;
	}
}
