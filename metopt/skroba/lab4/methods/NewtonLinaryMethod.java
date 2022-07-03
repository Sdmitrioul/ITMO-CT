package skroba.lab4.methods;

import skroba.lab1.methods.GoldenRatioMethod;
import skroba.lab3.matrix.ProfileMatrix;
import skroba.lab3.method.LUSolver;
import skroba.lab4.functions.FunctionData;
import skroba.utils.Vector;
import skroba.utils.matrix.SquareMatrix;

public class NewtonLinaryMethod extends AbstractMethod {
	public NewtonLinaryMethod(double EPS) {
		super(EPS, "Newton method with one dim optimization");
	}
	
	@Override
	public Vector findSolution(final Vector value, final FunctionData function) {
		Vector ans = value;
		ProfileMatrix profileMatrix;
		iterations = 0;
		final LUSolver solver = new LUSolver();
		logState(iterations + " " + ans  + " " + 0);
		while (true) {
			iterations++;
			Vector gradient = function.gradient(ans);
			SquareMatrix hessian = function.hessian(ans);
			profileMatrix = new ProfileMatrix(hessian);
			Vector d = solver.solve(profileMatrix, gradient.scalarMul(-1));
			final Vector tmp = ans;
			double r = (new GoldenRatioMethod(EPS, 0.95, y -> function.apply(tmp.sum(d.scalarMul(y))))).findMin(-100000, 100000).getMin();
			Vector s = d.scalarMul(r);
			ans = ans.sum(s);
			logState(iterations + " " + ans + " " + r);
			if (s.norma() <= EPS) {
				return ans;
			}
		}
	}
}
