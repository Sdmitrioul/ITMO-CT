package skroba.lab1.utils;

import skroba.lab1.utils.data.Answer;
import skroba.lab1.utils.data.Data;
import skroba.lab1.utils.exception.WrongDataException;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public final class BufferedAnswerWriter implements AutoCloseable {
	private BufferedWriter writer;
	private Answer answer;
	private Path root;
	
	public BufferedAnswerWriter(Path root, Answer answer) throws IOException {
		validArguments(root, answer);
		process();
	}
	
	private void validArguments(Path root, final Answer answer) throws WrongDataException {
		if (answer == null) {
			throw new WrongDataException("Answer can't be null");
		}
		if (root == null) {
			root = Paths.get(System.getProperty("user.dir")).resolve("src").resolve("skroba").resolve("lab1").resolve("results");
		}
		this.root = root;
		this.answer = answer;
	}
	
	private void process() throws IOException {
		Path outputPath = root.resolve(answer.getMethodName() + "Result.txt");
		if (outputPath.getParent() != null && !Files.exists(outputPath.getParent())) {
			Files.createDirectories(outputPath.getParent());
		}
		openFiles(outputPath);
	}
	
	private void openFiles(final Path outputPath) throws IOException {
		writer = Files.newBufferedWriter(outputPath);
		writer.write("Method name: " + answer.getMethodName().replace('_', ' '));
		writer.newLine();
		writer.write("The resulting minimum is " + doubleString(answer.getMin()));
		writer.newLine();
		writer.write("Count of operations " + answer.getData().size());
		writer.newLine();
		writeData();
		writer.newLine();
	}
	
	private void writeData() throws IOException {
	    String option = answer.getNameOfParameters();
	    String[] options = option.replaceAll("[()]", "").split(" ");
	    writer.write("\\begin{tabular}{");
	    writer.write(generatePar(options.length + 1) + "}\n\\hline\n");
	    writer.write("№ & ");
	    writer.write(String.join(" & ", options));
	    writer.write("\\\\ \\hline");
	    writer.newLine();
	    writer.write(data());
	    writer.write("\\\\ \\hline");
	    writer.write("\n\\end{tabular}");
	}
	
	private String data() {
		List<String> lines = new ArrayList<>();
		for (Map.Entry<Integer, Data> entry : answer.getData().entrySet()) {
			StringBuilder sb = new StringBuilder();
			sb.append(entry.getKey()).append(" & ")
					.append(doubleString(entry.getValue().leftBorder)).append(" & ")
					.append(doubleString(entry.getValue().rightBorder)).append(" & ");
			sb.append(entry.getValue().pairs.stream()
					.map(x -> doubleString(x.first) + " & " + doubleString(x.second))
					.collect(Collectors.joining(" & ")));
			lines.add(sb.toString());
		}
		return lines.stream().collect(Collectors.joining("\\\\ \\hline\n"));
	}
	
	public void write(String output) throws IOException {
		writer.write(output);
	}
	
	private String doubleString(double d) {
		return String.format("%.4f", d);
	}
	
	private String generatePar(int i) {
		StringBuilder sb = new StringBuilder();
		while (i-- > 0) {
			sb.append("|c");
		}
		return sb.toString() + "|";
	}
	
	@Override
	public void close() throws IOException {
		writer.close();
	}
}
