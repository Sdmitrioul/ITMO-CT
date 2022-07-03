import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Random;

public class Test {
    private static final int N = 10;
    private static Random random = new Random();
    public static void main(String[] args) {
        int[][] matrix = new int[N][N];
        for (int i = 0; i < N; i++) {
            Arrays.fill(matrix[i], 0);
        }
        /*for (int i = 0; i < 100; i++) {
            System.out.printf("%d ", random.nextInt(10));
        }*/
        for (int i = 0; i < N; i ++) {
            for (int j = i + 1; j < N; j++) {
                matrix[i][j] = random.nextInt(10) + 1;
                matrix[j][i] = matrix[i][j];
            }
        }
        show(matrix);
        int[][] d0 = fill(Arrays.copyOf(matrix, N));
        int[][] d1 = Arrays.copyOf(d0, N);

        for (int k = 0; k < N; k++) {
            for (int i = 0; i < N; i++) {
                for (int j = 0; j < N; j++) {
                    d0[i][j] = Math.min(d0[i][j], d0[i][k] + d0[k][j]);
                    d1[k][i] = Math.min(d0[k][i], d0[k][j] + d0[j][i]);
                }
            }
        }

        System.out.println("Good");
        show(d0);
        System.out.println("Bad");
        show(d1);
        System.out.println(Arrays.deepEquals(d0, d1));

    }

    private static int[][] fill(int[][] matrix) {
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix.length; j++) {
                if (matrix[i][j] == 0) matrix[i][j] = 100000;
            }
        }
        return matrix;
    }
    private static void show(int[][] matrix) {
        int size = matrix.length;
        for (int[] ints : matrix) {
            for (int j = 0; j < size; j++) {
                System.out.printf("%d ", ints[j]);
            }
            System.out.println();
        }
    }
}
