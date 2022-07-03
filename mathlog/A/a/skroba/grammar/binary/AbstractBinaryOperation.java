package a.skroba.grammar.binary;

import a.skroba.grammar.Expression;
import a.skroba.grammar.LogicSign;

public abstract class AbstractBinaryOperation implements Expression {
	public final LogicSign sign;
	public final Expression first;
	public final Expression second;
	
	public AbstractBinaryOperation(LogicSign sign, Expression first, Expression second) {
		this.sign = sign;
		this.first = first;
		this.second = second;
	}

	@Override
	public String toSimpleTreeGrammar() {
		return "(" + sign + ","
				+ first.toSimpleTreeGrammar()
				+ ","
				+ second.toSimpleTreeGrammar()
				+ ")";
	}
}
