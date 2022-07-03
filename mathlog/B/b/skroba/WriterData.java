package b.skroba;

import b.skroba.axioms.Axioms;
import b.skroba.axioms.ModusPonuns;
import b.skroba.grammar.Expression;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class WriterData implements AutoCloseable {
	private BufferedWriter writer;
	
	public WriterData() throws IOException {
		writer = new BufferedWriter(new OutputStreamWriter(System.out));
		//writer = Files.newBufferedWriter(Path.of("output"));
	}
	
	public void writeData(final List<Axioms> proofing, final List<Expression> hypothesis) throws IOException {
		write(hypothesis, proofing, proofing.get(proofing.size() - 1), 0);
	}
	
	private void write(final List<Expression> hypothesis, final List<Axioms> proofing, final Axioms axiom, final int number) throws IOException {
		if (axiom.isMP()) {
			ModusPonuns mp = (ModusPonuns) axiom;
			write(hypothesis, proofing, proofing.get(mp.leftRule), number + 1);
			write(hypothesis, proofing, proofing.get(mp.rightRule), number + 1);
		}
		
		writer.write(axiom.toNaturalOutput(hypothesis, number));
		writer.flush();
	}
	
	@Override
	public void close() throws IOException {
		writer.flush();
	}
}
