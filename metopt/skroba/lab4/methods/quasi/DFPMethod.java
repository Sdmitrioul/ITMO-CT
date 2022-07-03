package skroba.lab4.methods.quasi;

import skroba.utils.Vector;
import skroba.utils.matrix.Matrix;

public class DFPMethod extends AbstractQuasiNewtonMethod {
	public DFPMethod(final double EPS) {
		super(EPS, "DFPMethod");
	}
	
	@Override
	protected Matrix updateMatrix(Matrix G, Vector deltaGrad, Vector deltaX) {
		Vector v = G.mul(deltaGrad);
		return G.sub(deltaX
				.mulToMatrix(deltaX)
				.scalarMul(1 / deltaGrad.mul(deltaX)))
				.sub(v.mulToMatrix(v)
						.scalarMul(1/v
								.mul(deltaGrad)));
	}
}
