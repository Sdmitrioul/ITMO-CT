package skroba.lab4.methods;

import skroba.lab3.matrix.ProfileMatrix;
import skroba.lab3.method.LUSolver;
import skroba.lab4.functions.FunctionData;
import skroba.utils.Vector;
import skroba.utils.matrix.SquareMatrix;

public class NewtonMethod extends AbstractMethod {
	public NewtonMethod(double EPS) {
		super(EPS, "Newton Method");
	}
	
	@Override
	public Vector findSolution(final Vector value, final FunctionData function) {
		Vector ans = value;
		iterations = 0;
		final LUSolver solver = new LUSolver();
		logState(iterations + " " + ans);
		while (true) {
			iterations++;
			Vector gradient = function.gradient(ans);
			SquareMatrix hessian = function.hessian(ans);
			ProfileMatrix profileMatrix = new ProfileMatrix(hessian);
			Vector r = solver.solve(profileMatrix, gradient.scalarMul(-1));
			ans = ans.sum(r);
			logState(iterations + " " + ans);
			if (r.norma() <= EPS) {
				return ans;
			}
		}
	}
}
