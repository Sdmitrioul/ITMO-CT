package skroba.lab3;

import skroba.lab3.generators.Generator;
import skroba.lab3.generators.GeneratorDiagonal;
import skroba.lab3.generators.GeneratorGilbert;
import skroba.lab3.generators.GeneratorSimple;
import skroba.lab3.matrix.ProfileMatrix;
import skroba.lab3.method.GaussSolver;
import skroba.lab3.method.LUSolver;
import skroba.lab3.method.Solver;
import skroba.utils.Vector;
import skroba.utils.fileWriter.FileWriter;
import skroba.utils.fileWriter.FileWriterImpl;
import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Main {
	public static void main(String[] args) {
		//generateLU();
		testLU();
		//generateLU();
		//testCompare();
		//generateSimple();
		//testGilbert();
	}
	
	private static void testCompare() {
		try (final FileWriter writer = new FileWriterImpl("results/compare")) {
			writer.write("\\begin{tabular}{|c|c|c|c|c|c|c|c|}\n");
			writer.write("\\hline\n");
			writer.write("n & k & LU iterations & LU $ || x^{*} - x_{k} || LU $ & $ || x^{*} - x_{k} || / || x^{*} || $ & Gauss iterations & Gauss $ || x^{*} - x_{k} || LU $ & Gauss $ & $ || x^{*} - x_{k} || / || x^{*} || $\\\\ \\hline\n");
			Solver solver1 = new GaussSolver();
			Solver solver2 = new LUSolver();
			for (int i = 10; i <= 1000; i *= 10) {
				Vector x = new Vector(IntStream.range(1, i + 1).mapToDouble(Double::new).boxed().collect(Collectors.toList()));
				double norma = x.norma();
				for (int j = 1; j < 7; j++) {
					SquareMatrix matrix = MatrixReader.read(String.format("data/lu/%d/%d", i, j));
					
					Matrix copy = matrix.copy();
					Vector f = matrix.mul(x);
					
					Vector ansLU = solver2.solve(new ProfileMatrix(copy), f);
					Vector ansGauss = solver1.solve(matrix, f);
					
					long iter1 = solver2.getIterations();
					long iter2 = solver1.getIterations();
					
					double thirdPar = x.sum(ansLU.scalarMul(-1)).norma();
					double fourthPar = thirdPar / norma;
					
					double fifthPar = x.sum(ansGauss.scalarMul(-1)).norma();
					double sixPar = fifthPar / norma;
					
					writer.write(String.format("%d & %d & %d & %.12f & %.12f & %d & %.12f & %.12f \\\\ \\hline \n", i, j, iter1, thirdPar, fourthPar, iter2, fifthPar, sixPar));
				}
			}
			writer.write("\\end{tabular}");
		}
	}
	
	private static void testGilbert() {
		try (final FileWriter writer = new FileWriterImpl("results/simple")) {
			writer.write("\\begin{tabular}{|c|c|c|}\n");
			writer.write("\\hline\n");
			writer.write("n & $ || x^{*} - x_{k} || $ & $ || x^{*} - x_{k} || / || x^{*} || $ \\\\ \\hline\n");
			Solver solver = new GaussSolver();
			for (int i = 10; i < 1000; i += 40) {
				Vector x = new Vector(IntStream.range(1, i + 1).mapToDouble(Double::new).boxed().collect(Collectors.toList()));
				double norma = x.norma();
				
				SquareMatrix matrix = MatrixReader.read(String.format("data/simple/%d", i));
				
				Vector f = matrix.mul(x);
				Vector ans = solver.solve(matrix, f);
				
				double thirdPar = x.sum(ans.scalarMul(-1)).norma();
				double fourthPar = thirdPar / norma;
				writer.write(String.format("%d & %.12f & %.12f \\\\ \\hline \n", i, thirdPar, fourthPar));
			}
			writer.write("\\end{tabular}");
		}
	}
	
	private static void testLU() {
		try (final FileWriter writer = new FileWriterImpl("results/lu")) {
			writer.write("\\begin{tabular}{|c|c|c|c|}\n");
			writer.write("\\hline\n");
			writer.write("n & k & $ || x^{*} - x_{k} || $ & $ || x^{*} - x_{k} || / || x^{*} || $ \\\\ \\hline\n");
			Solver solver = new LUSolver();
			for (int i = 10; i <= 1000; i *= 10) {
				Vector x = new Vector(IntStream.range(1, i + 1).mapToDouble(Double::new).boxed().collect(Collectors.toList()));
				double norma = x.norma();
				for (int j = 1; j < 7; j++) {
					Matrix matrix = MatrixReader.read(String.format("data/lu/%d/%d", i, j));
					Vector f = matrix.mul(x);
					Vector ans = solver.solve(new ProfileMatrix(matrix), f);
					double thirdPar = x.sum(ans.scalarMul(-1)).norma();
					double fourthPar = thirdPar / norma;
					writer.write(String.format("%d & %d & %.12f & %.12f \\\\ \\hline \n", i, j, thirdPar, fourthPar));
				}
			}
			writer.write("\\end{tabular}");
		}
	}
	
	private static void generateGilbert() {
		Generator generatorDiagonal = new GeneratorGilbert();
		generatorDiagonal.generate("data/gilbert", 10, 1000, 50);
	}
	
	private static void generateLU() {
		GeneratorDiagonal generatorDiagonal = new GeneratorDiagonal();
		generatorDiagonal.generate("data/lu", 10, 1000, 10, 1, 7);
	}
	private static void generateSimple() {
		Generator generatorDiagonal = new GeneratorSimple();
		generatorDiagonal.generate("data/simple", 10, 1000, 40);
	}
}
