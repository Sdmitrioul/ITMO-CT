package skroba.lab4.functions;

import skroba.utils.Vector;
import skroba.utils.matrix.SquareMatrix;

public class QudraticFunction implements FunctionData {
	private final SquareMatrix A;
	private final Vector B;
	private final double c;
	
	public QudraticFunction(SquareMatrix a, Vector b, double c) {
		A = a;
		B = b;
		this.c = c;
	}
	
	@Override
	public double apply(Vector values) {
		return A.mul(values).mul(values) / 2 - B.mul(values) + c;
	}
	
	@Override
	public SquareMatrix hessian(Vector values) {
		return A;
	}
	
	@Override
	public int getSpan() {
		return A.size();
	}
	
	@Override
	public Vector gradient(Vector values) {
		return A.mul(values).sum(B.scalarMul(-1));
	}
}
