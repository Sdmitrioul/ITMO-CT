package skroba.lab4.methods.quasi;

import skroba.lab1.methods.GoldenRatioMethod;
import skroba.lab4.functions.FunctionData;
import skroba.lab4.methods.AbstractMethod;
import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;
import skroba.utils.matrix.SquareMatrix;

public abstract class AbstractQuasiNewtonMethod extends AbstractMethod {
	public AbstractQuasiNewtonMethod(double EPS, String METHOD_NAME) {
		super(EPS, METHOD_NAME);
	}
	
	protected abstract Matrix updateMatrix(Matrix G, Vector deltaGrad, Vector deltaX);
	
	@Override
	public Vector findSolution(Vector value, FunctionData function) {
		int iterations = 0;
		Vector x0 = value;
		Matrix G = new SquareMatrix(x0.size(), 1);
		Vector antiGrad = function.gradient(x0).scalarMul(-1);
		Vector p = antiGrad.copy();
		final Vector finalX1 = x0;
		final Vector finalP1 = p;
		double res = new GoldenRatioMethod(EPS, 0.95, y -> function.apply(finalX1.sum(finalP1.scalarMul(y)))).findMin(-10000, 10000).getMin();
		Vector x1 = x0.sum(p.scalarMul(res));
		Vector deltaX = x1.sum(x0.scalarMul(-1));
		logState(iterations + " " + x0);
		x0 = x1.copy();
		while (true) {
			iterations++;
			Vector antiGrad2 = function.gradient(x0).scalarMul(-1);
			Vector deltaGrad = antiGrad2.sum(antiGrad.scalarMul(-1));
			antiGrad = antiGrad2.copy();
			G = updateMatrix(G, deltaGrad, deltaX);
			p = G.mul(antiGrad2);
			final Vector finalX = x0;
			final Vector finalP = p;
			res = new GoldenRatioMethod(EPS, 0.95, y -> function.apply(finalX.sum(finalP.scalarMul(y)))).findMin(-10000, 10000).getMin();
			x1 = x0.sum(p.scalarMul(res));
			deltaX = x1.sum(x0.scalarMul(-1));
			x0 = x1;
			logState(iterations + " " + x0);
			if (deltaX.norma() <= EPS) {
				return x1;
			}
		}
	}
}
