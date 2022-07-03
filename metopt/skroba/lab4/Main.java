package skroba.lab4;

import skroba.lab2.methods.AbstractGradientIterator;
import skroba.lab2.methods.FastGradientMethod;
import skroba.lab4.functions.FunctionData;
import skroba.lab4.functions.QudraticFunction;
import skroba.lab4.methods.Method;
import skroba.lab4.methods.NewtonDescentMethod;
import skroba.lab4.methods.NewtonLinaryMethod;
import skroba.lab4.methods.NewtonMethod;
import skroba.lab4.methods.marquardt.MarquardtMethod_Version1;
import skroba.lab4.methods.marquardt.MarquardtMethod_Version2;
import skroba.lab4.methods.quasi.DFPMethod;
import skroba.lab4.methods.quasi.PowellMethod;
import skroba.utils.QuadraticFunctionCommon;
import skroba.utils.Vec;
import skroba.utils.Vector;
import skroba.utils.logger.Logger;
import skroba.utils.logger.LoggerImpl;
import skroba.utils.matrix.SquareMatrix;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.lang.Math.*;

public class Main {
	private final static double EPS = 0.00001;
	private static Vector gl = new Vector(IntStream.range(0, 100).mapToDouble(x -> 1.0).boxed().collect(Collectors.toList()));
	private final static Path root = Paths.get(System.getProperty("user.dir"))
			.resolve("src").resolve("skroba")
			.resolve("lab4").resolve("results");
	
	public static void main(String[] args) {
		//testNewtonMethodWithStartingVal();
		//testNewtonMethodsWithDifferentStartValues();
		//testQuasiNewtonMethods();
		testBonusMethods();
	}
	
	private static void testBonusMethods() {
		final FunctionData function = new FunctionData() {
			@Override
			public double apply(Vector values) {
				double ans = 0;
				for (int i = 0; i < 99; i++) {
					ans += 100 * pow(values.get(i + 1) - pow(values.get(i), 2), 2) + pow(1 - values.get(i), 2);
				}
				return ans;
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[100][100];
				
				hessian[0][0] = 1200 * pow(values.get(0), 2) - 400 * values.get(1) + 2;
				hessian[1][0] = hessian[0][1] = -400 * values.get(0);
				for (int i = 1; i < 99; i++) {
					hessian[i][i - 1] = -400 * values.get(i - 1);
					hessian[i][i] = 202 + 1200 * pow(values.get(i), 2) - 400 * values.get(i + 1);
					hessian[i][i + 1] = -400 * values.get(i);
				}
				hessian[98][99] = hessian[99][98] = -400 * values.get(98);
				hessian[99][99] = 200;
				return new SquareMatrix(hessian);
			}
			
			@Override
			public int getSpan() {
				return 100;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] ans = new double[100];
				ans[0] = -400 * (values.get(1) - pow(values.get(0), 2)) * values.get(0) - 2 * (1 - values.get(0));
				for (int i = 1; i < 98; i++) {
					ans[i] = -400 * (values.get(i + 1) - pow(values.get(i), 2)) * values.get(i) - 2 * (1 - values.get(i)) + 200 * (values.get(i) - pow(values.get(i - 1), 2));
				}
				ans[99] = 200 * (values.get(99) - pow(values.get(98), 2));
				return new Vector(ans);
			}
		};
		Vector start = new Vector(IntStream.range(0, 100).mapToDouble(x -> 5.0).boxed().collect(Collectors.toList()));
		
		final String task = "3";
		testMarquardtMethod_Version1(function, start, task);
		testMarquardtMethod_Version2(function, start, task);
		testNewtonLinaryMethod(function, start, task);
	}
	
	private static void testQuasiNewtonMethods() {
		//testFirstQuasiFunction();
		//testSecondQuasiFunction();
		//testThirdQuasiFunction();
		testFourthQuasiFunction();
	}
	
	private static void testFourthQuasiFunction() {
		final FunctionData function = new FunctionData() {
			@Override
			public double apply(Vector values) {
				return 100 - 2 / function1(values) - 1 / function2(values);
			}
			
			private double function2(Vector values) {
				return (1 + pow((values.get(0) - 2) / 2, 2) + pow((values.get(1) - 1) / 3, 2));
			}
			
			private double function1(Vector values) {
				return (1 + pow((values.get(0) - 1) / 2, 2) + pow((values.get(1) - 1) / 3, 2));
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[2][2];
				double zn1 = differential1(values);
				double zn2 = differential12(values);
				hessian[0][0] = -pow(values.get(0) - 2, 2) / (2 * pow(zn1, 3))
						- 2 * (pow(values.get(0) - 1, 2) / (2 * pow(zn2, 3)) - 1 / (2 * pow(zn2, 2)))
						+ 1 / (2 * pow(zn1, 2));
				hessian[0][1] = hessian[1][0] = -2 * (values.get(0) - 2) * (values.get(1) - 1) / (9 * pow(zn1, 3))
						- 4 * (values.get(0) - 1) * (values.get(1) - 1) / (9 * pow(zn2, 3));
				hessian[1][1] = -8 * pow(values.get(1) - 1, 2) / (81 * pow(zn1, 3))
						-16 * pow(values.get(1) - 1, 2) / (81 * pow(zn2, 3))
						+ 2 / (9 * pow(zn1, 2)) + 4 / (9 * pow(zn2, 2));
				return new SquareMatrix(hessian);
			}
			
			private double differential12(Vector values) {
				return  0.25 * pow(values.get(0) - 1, 2) + pow(values.get(1) - 1, 2) / 9 + 1;
			}
			
			private double differential1(Vector values) {
				return 0.25 * pow(values.get(0) - 2, 2) + pow(values.get(1) - 1, 2) / 9 + 1;
			}
			
			@Override
			public int getSpan() {
				return 2;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] gradient = new double[2];
				double zn1 = grad1(values);
				double zn2 = grad2(values);
				gradient[0] = (values.get(0) - 2) / (2 * zn1) + (values.get(0) - 1) / zn2;
				gradient[1] = 2 * (values.get(1) - 1) / (9 * zn1) + 4 * (values.get(1) - 1) / (9 * zn2);
				return new Vector(gradient);
			}
			
			private double grad2(Vector values) {
				return pow( 0.25 * pow(values.get(0) - 1, 2) + pow(values.get(1) - 1, 2) / 9 + 1, 2);
			}
			
			private double grad1(Vector values) {
				return pow(0.25 * pow(values.get(0) - 2, 2) + pow(values.get(1) - 1, 2) / 9 + 1, 2);
			}
		};
		Vector first = new Vector(new double[]{0, 0});
		Vector second = new Vector(new double[]{10, -1});
		Vector third = new Vector(new double[]{5, -10});
		
		final String task1 = "2_4_1";
		testPowellMethod(function, first, task1);
		testDFPMethod(function, first, task1);
		testNewtonLinaryMethod(function, first, task1);
		
		final String task2 = "2_4_2";
		testPowellMethod(function, second, task2);
		testDFPMethod(function, second, task2);
		testNewtonLinaryMethod(function, second, task2);
		
		final String task3 = "2_4_3";
		testPowellMethod(function, third, task3);
		testDFPMethod(function, third, task3);
		testNewtonLinaryMethod(function, third, task3);
	}
	
	private static void testThirdQuasiFunction() {
		final FunctionData function = new FunctionData() {
			@Override
			public double apply(Vector values) {
				return pow(values.get(0) + 10 * values.get(1), 2) + 5 * pow(values.get(2) - values.get(3), 2) + pow(values.get(1) - 2 * values.get(2), 4) + 10 * pow(values.get(0) - values.get(3), 4);
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[4][4];
				hessian[0][0] = 2 * (60 * pow(values.get(0) - values.get(3), 2) + 1);
				hessian[1][1] = 12 * pow((values.get(1) - 2 * values.get(2)) + 200, 2);
				hessian[2][2] = 48 * pow(values.get(1) - 2 * values.get(2), 2) + 10;
				hessian[3][3] = 120 * pow(values.get(0) - values.get(3), 2) + 10;
				
				hessian[1][0] = hessian[0][1] = 20;
				
				hessian[2][0] = hessian[0][2] = 0;
				
				hessian[3][0] = hessian[0][3] = -120 * pow(values.get(0) - values.get(3), 2);
				
				hessian[2][1] = hessian[1][2] = -24 * pow(values.get(1) - 2 * values.get(2), 2);
				
				hessian[3][1] = hessian[1][3] = 0;
				
				hessian[2][3] = hessian[3][2] = -10;
				
				return new SquareMatrix(hessian);
			}
			
			@Override
			public int getSpan() {
				return 2;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] gradient = new double[4];
				gradient[0] = 2 * (20 * pow(values.get(0) - values.get(3), 3) + values.get(0) + 10 * values.get(1));
				gradient[1] = 4 * (5 * (values.get(0) + 10 * values.get(1)) + pow(values.get(1) - 2 * values.get(2), 3));
				gradient[2] = 10 * (values.get(2) - values.get(3)) - 8 * pow(values.get(1) - 2 * values.get(2), 3);
				gradient[3] = 10 * (-4 * pow(values.get(0) - values.get(3), 3) + values.get(3) - values.get(2));
				return new Vector(gradient);
			}
		};
		Vector first = new Vector(new double[]{0.1, 0.1, 0.1, 0.1});
		Vector second = new Vector(new double[]{10, 10, 10, 10});
		Vector third = new Vector(new double[]{5, 5, 5, 5});
		
		final String task1 = "2_3_1";
		testPowellMethod(function, first, task1);
		testDFPMethod(function, first, task1);
		testNewtonLinaryMethod(function, first, task1);
		
		final String task2 = "2_3_2";
		testPowellMethod(function, second, task2);
		testDFPMethod(function, second, task2);
		testNewtonLinaryMethod(function, second, task2);
		
		final String task3 = "2_3_3";
		testPowellMethod(function, third, task3);
		testDFPMethod(function, third, task3);
		testNewtonLinaryMethod(function, third, task3);
	}
	
	private static void testSecondQuasiFunction() {
		final FunctionData function = new FunctionData() {
			@Override
			public double apply(Vector values) {
				return pow(pow(values.get(0), 2) + values.get(1) - 11, 2) + pow(pow(values.get(1), 2) + values.get(0) - 7, 2);
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[2][2];
				hessian[0][0] = 12 * pow(values.get(0), 2) + 4 * values.get(1) - 42;
				hessian[1][0] = hessian[0][1] = 4 * (values.get(0) + values.get(1));
				hessian[1][1] = 2 * (1 + 2 * ((values.get(0) + pow(values.get(1), 2) - 7) + 2 * pow(values.get(1), 2)));
				return new SquareMatrix(hessian);
			}
			
			@Override
			public int getSpan() {
				return 2;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] gradient = new double[2];
				gradient[0] = 2 * (2 * values.get(0) * (pow(values.get(0), 2) + values.get(1) - 11) + values.get(0) + pow(values.get(1), 2) - 7);
				gradient[1] = 2 * (pow(values.get(0), 2) + 2 * values.get(1) * (values.get(0) + pow(values.get(1), 2) - 7) + values.get(1) - 11);
				return new Vector(gradient);
			}
		};
		Vector first = new Vector(new double[]{0, 0});
		Vector second = new Vector(new double[]{10, 10});
		Vector third = new Vector(new double[]{5, 5});
		
		final String task1 = "2_2_1";
		testPowellMethod(function, first, task1);
		testDFPMethod(function, first, task1);
		testNewtonLinaryMethod(function, first, task1);
		
		final String task2 = "2_2_2";
		testPowellMethod(function, second, task2);
		testDFPMethod(function, second, task2);
		testNewtonLinaryMethod(function, second, task2);
		
		final String task3 = "2_2_3";
		testPowellMethod(function, third, task3);
		testDFPMethod(function, third, task3);
		testNewtonLinaryMethod(function, third, task3);
	}
	
	private static void testFirstQuasiFunction() {
		final FunctionData function = new FunctionData() {
			@Override
			public double apply(Vector values) {
				return 100 * pow(values.get(1) - pow(values.get(0), 2), 2) + pow(1 - values.get(0), 2);
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[2][2];
				hessian[0][0] = 1200 * pow(values.get(0), 2) - 400 * values.get(1) + 2;
				hessian[1][0] = hessian[0][1] = -400 * values.get(0);
				hessian[1][1] = 200;
				return new SquareMatrix(hessian);
			}
			
			@Override
			public int getSpan() {
				return 2;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] gradient = new double[2];
				gradient[0] = 2 * (200 * pow(values.get(0), 3) - 200 * values.get(0) * values.get(1) + values.get(0) - 1);
				gradient[1] = 200 * (values.get(1) - pow(values.get(0), 2));
				return new Vector(gradient);
			}
		};
		Vector first = new Vector(new double[]{0, 0});
		Vector second = new Vector(new double[]{10, 10});
		Vector third = new Vector(new double[]{5, 5});
		
		final String task1 = "2_1_1";
		testPowellMethod(function, first, task1);
		testDFPMethod(function, first, task1);
		testNewtonLinaryMethod(function, first, task1);
		
		final String task2 = "2_1_2";
		testPowellMethod(function, second, task2);
		testDFPMethod(function, second, task2);
		testNewtonLinaryMethod(function, second, task2);
		
		final String task3 = "2_1_3";
		testPowellMethod(function, third, task3);
		testDFPMethod(function, third, task3);
		testNewtonLinaryMethod(function, third, task3);
	}
	
	private static void testNewtonMethodsWithDifferentStartValues() {
		testFirst();
		testSecond();
	}
	
	private static void testSecond() {
		final FunctionData function = new FunctionData() {
			@Override
			public double apply(Vector values) {
				return pow((2 * values.get(0) + values.get(1) + 2), 4) + pow(values.get(0) - 6 * values.get(1), 2);
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[2][2];
				hessian[0][0] = 48 * pow(2 * values.get(0) + values.get(1) + 2, 2) + 2;
				hessian[1][0] = hessian[0][1] = 24 * pow(2 * values.get(0) + values.get(1) + 2, 2) - 12;
				hessian[1][1] = 12 * pow(2 * values.get(0) + values.get(1) + 2, 2) + 72;
				return new SquareMatrix(hessian);
			}
			
			@Override
			public int getSpan() {
				return 2;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] gradient = new double[2];
				gradient[0] = 2 * (values.get(0) - 6 * values.get(1) + 4 * pow(2 + 2 * values.get(0) + values.get(1), 3));
				gradient[1] = 4 * (-3 * (values.get(0) - 6 * values.get(1)) + pow(2 + 2 * values.get(0) + values.get(1), 3));
				return new Vector(gradient);
			}
		};
		Vector first = new Vector(new double[]{0.1, 0.1});
		Vector second = new Vector(new double[]{10, -1});
		Vector third = new Vector(new double[]{5, -10});
		
		testNewtonMethod(function, first, "1_1_2_1");
		testNewtonMethod(function, second, "1_1_2_2");
		testNewtonMethod(function, third, "1_1_2_3");
		
		testNewtonLinaryMethod(function, first, "1_1_2_1");
		testNewtonLinaryMethod(function, second, "1_1_2_2");
		testNewtonLinaryMethod(function, third, "1_1_2_3");
		
		testNewtonDescentMethod(function, first, "1_1_2_1");
		testNewtonDescentMethod(function, second, "1_1_2_2");
		testNewtonDescentMethod(function, third, "1_1_2_3");
	}
	
	private static void testFirst() {
		FunctionData function = new QudraticFunction(new SquareMatrix(new double[][]{{6, 6}, {6, 12}}), new Vector(new double[]{-6.0, 0.0}), 5.0);
		Vector first = new Vector(new double[]{0.1, 0.1});
		Vector second = new Vector(new double[]{1, 1});
		Vector third = new Vector(new double[]{15, 15});
		
		testNewtonMethod(function, first, "1_1_1_1");
		testNewtonMethod(function, second, "1_1_1_2");
		testNewtonMethod(function, third, "1_1_1_3");
		
		testNewtonLinaryMethod(function, first, "1_1_1_1");
		testNewtonLinaryMethod(function, second, "1_1_1_2");
		testNewtonLinaryMethod(function, third, "1_1_1_3");
		
		testNewtonDescentMethod(function, first, "1_1_1_1");
		testNewtonDescentMethod(function, second, "1_1_1_2");
		testNewtonDescentMethod(function, third, "1_1_1_3");
	}
	
	private static void testNewtonMethodWithStartingVal() {
		final FunctionData function_1_2_1 = new QudraticFunction(new SquareMatrix(new double[][]{{2, -1.2}, {-1.2, 2}}), new Vector(new double[]{0, 0}), 0);
		final Vector start_1_2_1 = new Vector(new double[]{4, 1});
		final FunctionData function_1_2_2 = new FunctionData() {
			@Override
			public double apply(Vector values) {
				return 100 * pow(values.get(1) - pow(values.get(0), 2), 2) + pow(1 - values.get(0), 2);
			}
			
			@Override
			public SquareMatrix hessian(Vector values) {
				double[][] hessian = new double[2][2];
				hessian[0][0] = 1200 * pow(values.get(0), 2) - 400 * values.get(1) + 2;
				hessian[1][0] = hessian[0][1] = -400 * values.get(0);
				hessian[1][1] = 200;
				return new SquareMatrix(hessian);
			}
			
			@Override
			public int getSpan() {
				return 2;
			}
			
			@Override
			public Vector gradient(Vector values) {
				double[] gradient = new double[2];
				gradient[0] = 2 * (200 * pow(values.get(0), 3) - 200 * values.get(0) * values.get(1) + values.get(0) - 1);
				gradient[1] = 200 * (values.get(1) - pow(values.get(0), 2));
				return new Vector(gradient);
			}
		};
		final Vector start_1_2_2 = new Vector(new double[]{-1.2, 1});
		
		String task1 = "1_2_1";
		testNewtonMethod(function_1_2_1, start_1_2_1, task1);
		testNewtonLinaryMethod(function_1_2_1, start_1_2_1, task1);
		testNewtonDescentMethod(function_1_2_1, start_1_2_1, task1);
		//testFastGradientMethod(function_1_2_1, start_1_2_1, task1);
		
		String task2 = "1_2_2";
		testNewtonMethod(function_1_2_2, start_1_2_2, task2);
		testNewtonLinaryMethod(function_1_2_2, start_1_2_2, task2);
		testNewtonDescentMethod(function_1_2_2, start_1_2_2, task2);
		//testFastGradientMethod(function_1_2_2, start_1_2_2, task2);
	}
	
	private static void testMarquardtMethod_Version2(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("MarquardtMethod_Version2.txt"), true);
		Method newton = new MarquardtMethod_Version2(EPS);
		newton.setLogger(newtonMethodLogger);
		Vector ans = newton.findSolution(startValue, function);
		/*System.out.println("MarquardtMethod_Version2----------");
		double s = ans.sum(gl.scalarMul(-1)).norma();
		System.out.println(s);
		System.out.println(s / gl.norma());
		System.out.println("----------------------------------");*/
		newtonMethodLogger.close();
	}
	
	private static void testMarquardtMethod_Version1(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("MarquardtMethod_Version1.txt"), true);
		Method newton = new MarquardtMethod_Version1(EPS);
		newton.setLogger(newtonMethodLogger);
		Vector ans = newton.findSolution(startValue, function);
		/*System.out.println("MarquardtMethod_Version1----------");
		double s = ans.sum(gl.scalarMul(-1)).norma();
		System.out.println(s);
		System.out.println(s / gl.norma());
		System.out.println("----------------------------------");*/
		newtonMethodLogger.close();
	}
	
	private static void testDFPMethod(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("DFPMethod.txt"), true);
		Method newton = new DFPMethod(EPS);
		newton.setLogger(newtonMethodLogger);
		newton.findSolution(startValue, function);
		newtonMethodLogger.close();
	}
	
	private static void testPowellMethod(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("PowellMethod.txt"), true);
		Method newton = new PowellMethod(EPS);
		newton.setLogger(newtonMethodLogger);
		newton.findSolution(startValue, function);
		newtonMethodLogger.close();
	}
	
	private static void testFastGradientMethod(final FunctionData function, final Vector startValue, final String task) {
		final Logger fastGradientMethodLogger = new LoggerImpl(root.resolve(task).resolve("FastGradientMethod.txt"), true);
		AbstractGradientIterator iterator = new FastGradientMethod(new QuadraticFunctionCommon(function));
		int iter = -1;
		while (iterator.hasNext()) {
			iter++;
			fastGradientMethodLogger.log(iter + " " + iterator.getCurrentValue().getFirst() + "\n");
			iterator.next();
		}
		fastGradientMethodLogger.log(iter + " " + iterator.getCurrentValue().getFirst() + "\n");
		fastGradientMethodLogger.close();
	}
	
	private static void testNewtonMethod(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("NewtonMethod.txt"), true);
		Method newton = new NewtonMethod(EPS);
		newton.setLogger(newtonMethodLogger);
		newton.findSolution(startValue, function);
		newtonMethodLogger.close();
	}
	
	private static void testNewtonLinaryMethod(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("NewtonLinaryMethod.txt"), true);
		Method newton = new NewtonLinaryMethod(EPS);
		newton.setLogger(newtonMethodLogger);
		Vector ans = newton.findSolution(startValue, function);
		/*System.out.println("NewtonLinaryMethod----------");
		double s = ans.sum(gl.scalarMul(-1)).norma();
		System.out.println(s);
		System.out.println(s / gl.norma());
		System.out.println("----------------------------------");*/
		newtonMethodLogger.close();
	}
	
	private static void testNewtonDescentMethod(final FunctionData function, final Vector startValue, final String task) {
		final Logger newtonMethodLogger = new LoggerImpl(root.resolve(task).resolve("NewtonDescentMethod.txt"), true);
		Method newton = new NewtonDescentMethod(EPS);
		newton.setLogger(newtonMethodLogger);
		newton.findSolution(startValue, function);
		newtonMethodLogger.close();
	}
}
