package a.skroba.grammar;

public final class Variable implements Expression {
	private final String name;
	
	public Variable(final String name) {
		this.name = name;
	}
	
	@Override
	public String toSimpleTreeGrammar() {
		return name;
	}
}
