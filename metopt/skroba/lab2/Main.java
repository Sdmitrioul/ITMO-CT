package skroba.lab2;


import skroba.lab2.methods.AbstractGradientIterator;
import skroba.lab2.methods.ConjugateGradients;
import skroba.lab2.methods.FastGradientMethod;
import skroba.lab2.methods.GradientMethod;
import skroba.utils.*;
import skroba.utils.fileWriter.FileWriter;
import skroba.utils.fileWriter.FileWriterImpl;
import skroba.utils.logger.GradientLoggerImpl;
import skroba.utils.matrix.SquareMatrix;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Main {
	public static void main(String[] args) {
		testFirstFun();
		testSecondFun();
		testAllIterations();
	}
	
	private static void testAllIterations() {
		testMethod(GradientMethod.class, "gradient");
		testMethod(FastGradientMethod.class, "fast");
		testMethod(ConjugateGradients.class, "conjugate");
	}
	
	private static void testMethod(final Class<? extends AbstractGradientIterator> token, final String fileName) {
		try {
			final Constructor<? extends AbstractGradientIterator> constructor = token == ConjugateGradients.class  ?  token.getConstructor(QuadraticFunctionCommon.class, int.class) :  token.getConstructor(QuadraticFunctionCommon.class);
			testIterations(constructor,fileName, 1000);
		} catch (NoSuchMethodException ex) {
			System.err.println("Can't find constructor: " + ex.getMessage());
		} catch (InvocationTargetException ex) {
			System.err.println("Can't create new instance of gradient method: " + ex.getMessage());
		} catch (InstantiationException ex) {
			System.err.println("Some exception: " + ex.getMessage());
		} catch (IllegalAccessException ex) {
			System.err.println("Can't get access to constructor: " + ex.getMessage());
		}
	}
	
	private static void testIterations(final Constructor<? extends AbstractGradientIterator> constructor, final String fileName, final int n, int ... otherArgs) throws InvocationTargetException, InstantiationException, IllegalAccessException {
		try	(FileWriter fileWriter = new FileWriterImpl("results/" + fileName + "/" + n)) {
			fileWriter.write("k iterations\n");
			for (int i = 1; i <= 3125; i *= 5) {
				double result = 0.0;
				for (int j = 0; j < 3; j++) {
					result += iterateMethod(constructor, Generator.generate(n, i), otherArgs).getSecond().size();
					result += iterateMethod(constructor, Generator.generateDiag(n, i), otherArgs).getSecond().size();
				}
				fileWriter.write(String.format("%d %.8f", i, result / 6));
				fileWriter.write("\n");
				fileWriter.flush();
			}
		}
	}
	
	private static void testFirstFun() {
		final SquareMatrix aMatrixFirst = new SquareMatrix(List.of(List.of(32.0, -20.0), List.of(-20.0, 16.0)));
		final Vector bVectorFirst = new Vector(List.of(5.0, -7.0));
		final QuadraticFunctionCommon funFirst = new QuadraticFunctionCommon(aMatrixFirst, bVectorFirst, 1);
		
		try {
			testAllMethods(funFirst, "first-fun");
		} catch (NoSuchMethodException ex) {
			System.err.println("Can't find constructor: " + ex.getMessage());
		} catch (InvocationTargetException ex) {
			System.err.println("Can't create new instance of gradient method: " + ex.getMessage());
		} catch (InstantiationException ex) {
			System.err.println("Some exception: " + ex.getMessage());
		} catch (IllegalAccessException ex) {
			System.err.println("Can't get access to constructor: " + ex.getMessage());
		}
	}
	
	private static void testSecondFun() {
		final SquareMatrix aMatrixSecond = new SquareMatrix(List.of(List.of(2.0, 0.0), List.of(0.0, 6.0)));
		final Vector bVectorSecond = new Vector(List.of(-20.0, -12.0));
		final QuadraticFunctionCommon funSecond = new QuadraticFunctionCommon(aMatrixSecond, bVectorSecond, 5);
		
		try {
			testAllMethods(funSecond, "second-fun");
		} catch (NoSuchMethodException ex) {
			System.err.println("Can't find constructor: " + ex.getMessage());
		} catch (InvocationTargetException ex) {
			System.err.println("Can't create new instance of gradient method: " + ex.getMessage());
		} catch (InstantiationException ex) {
			System.err.println("Some exception: " + ex.getMessage());
		} catch (IllegalAccessException ex) {
			System.err.println("Can't get access to constructor: " + ex.getMessage());
		}
	}
	
	private static void testAllMethods(final QuadraticFunctionCommon fun, final String ... dir) throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
		final String prefix = Arrays.stream(dir).collect(Collectors.joining("/")) + "/";
		testMethod(GradientMethod.class.getConstructor(QuadraticFunctionCommon.class), prefix + "GradientMethod", fun);
		testMethod(FastGradientMethod.class.getConstructor(QuadraticFunctionCommon.class), prefix + "FastGradientMethod", fun);
		testMethod(ConjugateGradients.class.getConstructor(QuadraticFunctionCommon.class, int.class), prefix + "ConjugateGradientMethod", fun, 10);
	}
	
	private static void testMethod(final Constructor<? extends AbstractGradientIterator> constructor, final String fileName, final QuadraticFunctionCommon fun, int ... otherArgs) throws InvocationTargetException, InstantiationException, IllegalAccessException {
		final Pair<String, List<Pair<Vector, Double>>> output = iterateMethod(constructor, fun, otherArgs);
		final String name = output.getFirst();
		final List<Pair<Vector, Double>> data = output.getSecond();
		
		try	(FileWriter fileWriter = new FileWriterImpl("results/" + fileName)) {
			fileWriter.write(name + "\n");
			fileWriter.write("Count of iterations: " + data.size() + "\n");
			fileWriter.write("Result: " + data.get(data.size() - 1).toString() + "\n");
			fileWriter.write("Iterations: \n");
			fileWriter.write(
					data.stream()
					/*.map(x -> {
						final String numbers = x.getFirst().toString().replaceAll("[\\[\\]]", "");
						return String.format("%s %.8f", numbers, x.getSecond()).replaceAll("[,]", ".");
					})*/
							.map(Pair::toString)
					.collect(Collectors.joining("\n"))
			);
			fileWriter.write("\n");
		}
	}
	
	private static Pair<String, List<Pair<Vector, Double>>> iterateMethod(final Constructor<? extends AbstractGradientIterator> constructor, final QuadraticFunctionCommon fun, int ... otherArgs) throws InvocationTargetException, InstantiationException, IllegalAccessException {
		AbstractGradientIterator iterator = otherArgs.length == 0 ? constructor.newInstance(fun) : constructor.newInstance(fun, otherArgs[0]);
		
		iterator.setLogger(new GradientLoggerImpl());
		
		while (iterator.hasNext()) {
			iterator.next();
		}
		
		return new Pair<>(iterator.getMethodName(), iterator.getLogs());
	}
}

