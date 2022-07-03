package skroba.lab4.functions;


import skroba.utils.Vector;
import skroba.utils.matrix.SquareMatrix;

public interface FunctionData {
	double apply(Vector values);
	
	SquareMatrix hessian(Vector values);
	
	int getSpan();
	
	Vector gradient(Vector values);
}
