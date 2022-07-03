package skroba.lab4.methods.marquardt;

import skroba.lab3.method.Solver;
import skroba.lab4.functions.FunctionData;
import skroba.lab4.methods.AbstractMethod;
import skroba.lab4.methods.marquardt.helper.HoleckySolver;
import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

public class MarquardtMethod_Version2 extends AbstractMethod {
	private final double LAMBDA = 0D;
	private final double BETA = 2;
	public MarquardtMethod_Version2(double EPS) {
		super(EPS, "MarquardtMethod_Version2");
	}
	
	public MarquardtMethod_Version2() {
		this(0.000001);
	}
	
	@Override
	public Vector findSolution(Vector value, FunctionData function) {
		iterations = 0;
		Matrix I = new SquareMatrix(value.size(), 1);
		Vector x = value;
		Solver<SquareMatrix> solver = new HoleckySolver();
		double step = LAMBDA;
		logState(iterations + " " + x + " " + step);
		while (true) {
			iterations++;
			Vector antiGrad = function.gradient(x).scalarMul(-1);
			SquareMatrix hessian = function.hessian(x);
			int counter = 0;
			Vector direction;
			while (true) {
				counter++;
				direction = solver.solve(hessian.sum(I.scalarMul(step)), antiGrad);
				if (direction != null) {
					x = x.sum(direction);
					break;
				}
				step = Math.max(1, BETA * step);
			}
			
			logState(iterations + " " + x+  " " + counter);
			
			if (direction.norma() <= EPS) {
				return x;
			}
		}
	}
}
