package skroba.lab4.methods.quasi;

import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;

public class PowellMethod extends AbstractQuasiNewtonMethod {
	public PowellMethod(double EPS) {
		super(EPS, "PowellMethod");
	}
	
	@Override
	protected Matrix updateMatrix(Matrix G, Vector deltaGrad, Vector deltaX) {
		Vector deltaXK = deltaX.sum(G.mul(deltaGrad));
		return G.sub(deltaXK.mulToMatrix(deltaXK).scalarMul(1/deltaGrad.mul(deltaXK)));
	}
}
