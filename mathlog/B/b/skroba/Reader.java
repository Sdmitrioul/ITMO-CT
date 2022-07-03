package b.skroba;

import b.skroba.grammar.Expression;
import b.skroba.utils.Pair;
import b.skroba.utils.Scanner;
import b.skroba.utils.parser.ExpressionParser;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Reader {
	public static Pair<Proofing, List<Expression>> readInput() {
		try (final Scanner scanner = new Scanner(System.in)) {
			if (!scanner.hasLine()) {
				return null;
			}
			
			final String[] firstLine = scanner.getLine().split("\\|-");
			final List<Expression> hypothesis = new ArrayList<>();
			
			if (!firstLine[0].isEmpty()) {
				hypothesis.addAll(Arrays.stream(firstLine[0].split(","))
						.map(ExpressionParser::parse).collect(Collectors.toList()));
			}
			
			final Expression proofing = ExpressionParser.parse(firstLine[1]);
			
			final List<Expression> proof = new ArrayList<>();
			
			while (scanner.hasLine()) {
				String line = scanner.getLine();
				if (line.isEmpty()) {
					break;
				}
				proof.add(ExpressionParser.parse(line));
			}
			
			return new Pair<>(new Proofing(hypothesis, proofing), proof);
		} catch (IOException ex) {
			throw new RuntimeException("Problem while reading input data, exception: " + ex.getMessage());
		}
	}
	
	public static Pair<Proofing, List<Expression>> readFile() {
		try (final BufferedReader scanner = Files.newBufferedReader(Path.of("40.crash"))) {
			
			final String[] firstLine = scanner.readLine().split("\\|-");
			final List<Expression> hypothesis = new ArrayList<>();
			
			if (!firstLine[0].isEmpty()) {
				hypothesis.addAll(Arrays.stream(firstLine[0].split(","))
						.map(ExpressionParser::parse).collect(Collectors.toList()));
			}
			
			final Expression proofing = ExpressionParser.parse(firstLine[1]);
			
			final List<Expression> proof = new ArrayList<>();
			while (true) {
				final String line = scanner.readLine();
				if (line == null || line.isEmpty()) {
					break;
				}
				proof.add(ExpressionParser.parse(line));
			}
			
			return new Pair<>(new Proofing(hypothesis, proofing), proof);
		} catch (IOException ex) {
			throw new RuntimeException("Problem while reading input data, exception: " + ex.getMessage());
		}
	}
}
