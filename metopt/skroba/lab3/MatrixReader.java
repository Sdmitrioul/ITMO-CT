package skroba.lab3;

import skroba.utils.matrix.SquareMatrix;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Class with static method that reads matrix from file and returns {@link SquareMatrix}.
 */
public class MatrixReader {
	public static SquareMatrix read(final String filename) {
		try {
			final Path path = Paths.get(filename);
			try (final BufferedReader reader = Files.newBufferedReader(path)) {
				final int size = Integer.parseInt(reader.readLine());
				final List<List<Double>> matrix = new ArrayList<>(size);
				for (int i = 0; i < size; i++) {
					matrix.add(
							Arrays.stream(reader.readLine().replaceAll(",", ".").split(" "))
									.map(Double::parseDouble).collect(Collectors.toList()));
				}
				return new SquareMatrix(matrix);
			} catch (IOException ex) {
				System.err.println("Can't open or read matrix from file: " + ex.getMessage());
			}
		} catch (InvalidPathException ex) {
			System.err.println("Can't find path: " + filename + " - exception: " + ex.getMessage());
		}
		return null;
	}
}
