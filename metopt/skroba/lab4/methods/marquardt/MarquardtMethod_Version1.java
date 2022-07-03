package skroba.lab4.methods.marquardt;

import skroba.lab1.methods.BrentCombineMethod;
import skroba.lab3.method.GaussSolver;
import skroba.lab3.method.Solver;
import skroba.lab4.functions.FunctionData;
import skroba.lab4.methods.AbstractMethod;
import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

public class MarquardtMethod_Version1 extends AbstractMethod {
	private final double BETA = 0.5;
	private final double LAMBDA = 1000000000000000D;
	public MarquardtMethod_Version1(double EPS) {
		super(EPS, "MarquardtMethod_Version1");
	}
	
	public MarquardtMethod_Version1() {
		super(0.0000001, "MarquardtMethod_Version1");
	}
	
	@Override
	public Vector findSolution(final Vector value, final FunctionData function) {
		iterations = 0;
		Matrix I = new SquareMatrix(value.size(), 1);
		Vector x = value;
		Solver<SquareMatrix> solver = new GaussSolver();
		double step = LAMBDA;
		logState(iterations + " " + x + " " + step);
		while (true) {
			iterations++;
			Vector antiGrad = function.gradient(x).scalarMul(-1);
			Vector direction = solver.solve(function.hessian(x).sum(I.scalarMul(step)), antiGrad);
			final Vector finalX = x;
			double alpha = new BrentCombineMethod(EPS, BETA, y -> function.apply(finalX.sum(direction.scalarMul(y)))).findMin(-100000, 100000).getMin();
			Vector optimal = direction.scalarMul(alpha);
			x = x.sum(optimal);
			logState(iterations + " " + x + " " + step);
			step *= BETA;
			if (optimal.norma() <= EPS) {
				return x;
			}
		}
	}
}
