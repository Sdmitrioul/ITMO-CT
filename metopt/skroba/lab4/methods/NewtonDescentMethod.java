package skroba.lab4.methods;

import skroba.lab1.methods.GoldenRatioMethod;
import skroba.lab3.matrix.ProfileMatrix;
import skroba.lab3.method.LUSolver;
import skroba.lab4.functions.FunctionData;
import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;

public class NewtonDescentMethod extends AbstractMethod {
	public NewtonDescentMethod(double EPS) {
		super(EPS, "Newton descent optimization");
	}
	
	@Override
	public Vector findSolution(final Vector value, final FunctionData function) {
		Vector ans = value;
		iterations = 0;
		Vector antiGrad = function.gradient(ans).scalarMul(-1);
		Vector finalAntiGrad = antiGrad;
		double alpha0 = new GoldenRatioMethod(EPS, 0.95, y -> function.apply(value.sum(finalAntiGrad.scalarMul(y)))).findMin(-10000, 10000).getMin();
		Vector x = antiGrad.scalarMul(alpha0);
		logState(iterations + " " + ans);
		ans = value.sum(x);
		final LUSolver solver = new LUSolver();
		while (true) {
			iterations++;
			Vector gradient = function.gradient(ans);
			Matrix hessian = function.hessian(ans);
			x = solver.solve(new ProfileMatrix(hessian), gradient.scalarMul(-1));
			antiGrad = x.mul(gradient) < 0 ? x : gradient.scalarMul(-1);
			Vector finalAntiGrad1 = antiGrad;
			Vector finalX = ans;
			alpha0 = new GoldenRatioMethod(EPS, 0.95, y -> function.apply(finalX.sum(finalAntiGrad1.scalarMul(y)))).findMin(-100000, 100000).getMin();
			x = antiGrad.scalarMul(alpha0);
			ans = ans.sum(x);
			logState(iterations + " " + ans);
			if (x.norma() <= EPS) {
				return ans;
			}
		}
	}
}
