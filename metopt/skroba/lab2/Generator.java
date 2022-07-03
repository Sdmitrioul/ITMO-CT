package skroba.lab2;

import skroba.utils.QuadraticFunctionCommon;
import skroba.utils.matrix.SquareMatrix;
import skroba.utils.Vector;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class Generator {
	private static Random random = new Random();
	public static QuadraticFunctionCommon generate(final int n, final int k) {
		List<Double> diag = createDiagonal(n, k);
		Collections.shuffle(diag);
		List<List<Double>> aMatrix = getMatrix(n, diag);
		
		return new QuadraticFunctionCommon(new SquareMatrix(aMatrix), new Vector(Collections.nCopies(n, 0.0)), 0);
	}
	
	public static QuadraticFunctionCommon generateDiag(final int n, final int k) {
		List<Double> diag = createDiagonal(n, k);
		
		List<List<Double>> aMatrix = getMatrix(n, diag);
		
		return new QuadraticFunctionCommon(new SquareMatrix(aMatrix), new Vector(Collections.nCopies(n, 0.0)), 0);
	}
	
	private static List<Double> createDiagonal(final int n, final int k) {
		List<Double> diag = new ArrayList<>(n);
		diag.add((double) k);
		diag.add((double) 1);
		for (int i = 0; i < n - 2; i++) {
			diag.add((random.nextDouble() * (k + 1)) % (k + 1));
		}
		diag.add((double) k);
		return diag;
	}
	
	private static List<List<Double>> getMatrix(int n, List<Double> diag) {
		int increment = 0;
		List<List<Double>> aMatrix = new ArrayList<>(n);
		for (int i = 0; i < n; i++) {
			aMatrix.add(new ArrayList<>());
			for (int j = 0; j < n; j++) {
				if (i == j) {
					aMatrix.get(i).add(diag.get(increment++));
					continue;
				}
				
				aMatrix.get(i).add(0.0);
			}
		}
		return aMatrix;
	}
}
