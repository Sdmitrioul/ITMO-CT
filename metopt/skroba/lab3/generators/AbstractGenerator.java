package skroba.lab3.generators;

import skroba.utils.matrix.Matrix;
import skroba.utils.fileWriter.FileWriter;
import skroba.utils.fileWriter.FileWriterImpl;

/**
 * Abstract class for {@link Generator} interface.
 */
public abstract class AbstractGenerator implements Generator{
	@Override
	public void writeInFile(Matrix matrix, String fileName) {
		try (FileWriter writer = new FileWriterImpl(fileName)) {
			writer.write(matrix.size() + "\n");
			for (int i = 0; i < matrix.size(); i++) {
				for (int j = 0; j < matrix.size(); j++) {
					writer.write(String.format("%.10f ", matrix.getElement(i, j)));
				}
				writer.write("\n");
			}
		}
	}
	
	@Override
	public void generate(final String prefix, final int from, final int to, final int step) {
		for (int i = from; i <= to; i += step) {
			writeInFile(generate(i), prefix + "/" + i);
		}
	}
}
