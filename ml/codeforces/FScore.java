import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class FScore {
	private static int all = 0;
	public static void main(String[] args) {
		try (final Scanner scanner = new Scanner(System.in)) {
			if (scanner.hasLine() && scanner.hasNextInt()) {
				final int size = scanner.getNextInt();
				
				int[][] matrix = readMatrix(scanner, size);
				
				double[] result = calculateFScores(matrix);
				
				for (int i = 0; i < 3; i++) {
					System.out.printf("%,.7f%n", result[i]);
				}
			}
		} catch (IOException ex) {
			System.err.println("Something wrong in input");
		}
	}
	
	private static double[] calculateFScores(int[][] matrix) {
		final double[] result = new double[3];
		
		result[0] = fMeasure(matrix);
		result[1] = fmacro(matrix);
		result[2] = fmicro(matrix);
		
		return result;
	}
	
	private static double fmicro(int[][] matrix) {
		double ans = 0;
		
		for (int i = 0; i < matrix.length; i++) {
			ans += rowSum(matrix, i) * fmi(matrix, i);
		}
		
		return ans / all;
	}
	
	private static double fmi(int[][] matrix, int i) {
		double recall = 0;
		double precision = 0;
		
		for (int[] ints : matrix) {
			precision += ints[i];
		}
		
		for (int j = 0; j < matrix.length; j++) {
			recall += matrix[i][j];
		}
		
		precision = precision == 0 ? 1 : matrix[i][i] / precision;
		recall = recall == 0 ? 1 : matrix[i][i] / recall;
		
		return precision + recall == 0 ? 0 : 2 * precision * recall / (precision + recall);
	}
	
	private static double fmacro(int[][] matrix) {
		final double precision = calculatePrecision(matrix);
		final double recall = calculateRecall(matrix);
		
		return precision + recall != 0 ? 2 * precision * recall / (precision + recall) : 0;
	}
	
	private static double fMeasure(int[][] matrix) {
		double tp = 0;
		double fp = 0;
		double fn = 0;
		
		for (int i = 0; i < matrix.length; i++) {
			double k = (double) rowSum(matrix, i) / all;
			
			tp += matrix[i][i] * k;
			fp += (colSum(matrix, i) - matrix[i][i]) * k;
			fn += (rowSum(matrix, i) - matrix[i][i]) * k;
		}
		double precision = tp + fp == 0 ? 0 : tp / (tp + fp);
		double recall = tp + fn == 0 ? 0 : tp / (tp + fn);
		
		return precision + recall == 0 ? 0 : 2 * recall * precision / (recall + precision);
	}
	
	private static double calculatePrecision(int[][] matrix) {
		final int size = matrix.length;
		
		double ans = 0;
		
		for (int i = 0; i < size; i++) {
			var el = matrix[i][i];
			ans += el == 0 ? 0 : (double) el * rowSum(matrix, i) / colSum(matrix, i);
		}
		
		return ans / all;
	}
	
	private static double calculateRecall(final int[][] matrix) {
		final int size = matrix.length;
		
		double ans = 0;
		int tp = 0;
		
		for (int i = 0; i < size; i++) {
			var el = matrix[i][i];
			tp += el;
			ans += el == 0 ? 0 : (double) el / rowSum(matrix, i);
		}
		
		return (double) tp / all;
	}
	
	private static int rowSum(int[][] matrix, int row) {
		final int length = matrix[row].length;
		
		int ans = 0;
		
		for (int i = 0; i < length; i++) {
			ans += matrix[row][i];
		}
		
		return ans;
	}
	
	private static int colSum(int[][] matrix, int col) {
		int ans = 0;
		
		for (int[] row : matrix) {
			ans += row[col];
		}
		
		return ans;
	}
	
	private static int[][] readMatrix(final Scanner scanner, final int size) throws IOException {
		int[][] matrix = new int[size][size];
		
		for (int i = 0; i < size; i++) {
			if (!scanner.hasLine()) continue;
			
			for (int j = 0; j < size; j++) {
				if (!scanner.hasNextInt()) continue;
				
				matrix[i][j] = scanner.getNextInt();
				all += matrix[i][j];
			}
		}
		
		return matrix;
	}
	
	private static class Scanner implements AutoCloseable {
		private BufferedReader reader;
		private String line;
		private boolean readed;
		private int index = 0;
		private int position = 0;
		private String mark;
		
		public Scanner(InputStream in) throws UnsupportedEncodingException {
			try{
				reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8.name()));
			} catch (UnsupportedEncodingException e){
				throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
			}
		}
		
		public Scanner(String s) throws FileNotFoundException, UnsupportedEncodingException {
			try{
				reader = new BufferedReader(new InputStreamReader(new FileInputStream(s), StandardCharsets.UTF_8.name()));
			} catch (FileNotFoundException e){
				throw new FileNotFoundException("File not found :" + e.getMessage());
			} catch (UnsupportedEncodingException e){
				throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
			}
		}
		
		public boolean hasNextInt() {
			while (index < line.length() && Character.isWhitespace(line.charAt(index))){
				index++;
			}
			return index != line.length();
		}
		
		public boolean hasNextWord() {
			while (index < line.length() && !isWordSymbol(line.charAt(index))){
				//System.out.println(line.charAt(index) + " " + Character.isLetter(line.charAt(index)));
				index++;
			}
			return index != line.length();
		}
		
		private boolean isWordSymbol(char c) {
			return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION ||  c == '\'';
		}
		
		public int getNextInt() {
			if (!hasNextInt()) {
				throw new NoSuchElementException();
			}
			int begin = index;
			while (!Character.isWhitespace(line.charAt(index))){
				index++;
				if (index == line.length()){
					break;
				}
			}
			return Integer.parseInt(line.substring(begin, index));
		}
		
		public long getNextLong() {
			if (!hasNextInt()) {
				throw new NoSuchElementException();
			}
			int begin = index;
			while (!Character.isWhitespace(line.charAt(index))){
				index++;
				if (index == line.length()){
					break;
				}
			}
			return Long.parseLong(line.substring(begin, index));
		}
		
		public String getNextWord() {
			if (!hasNextWord()) {
				throw new NoSuchElementException();
			}
			int begin = index;
			while (isWordSymbol(line.charAt(index))){
				index++;
				if (index == line.length()){
					break;
				}
			}
			return line.substring(begin, index);
		}
		
		public boolean hasLine() throws IOException {
			try{
				index = 0;
				readed = true;
				line = reader.readLine();
			} catch (IOException e) {
				throw new IOException("Input error in Scanner.hasLine(): " + e.getMessage());
			}
			return line != null;
		}
		
		public String getLine() throws IOException {
			if (readed) {
				readed = false;
				return line;
			} else {
				if (hasLine()) {
					readed = false;
					return line;
				} else {
					return "";
				}
			}
		}
		
		public void close() throws IOException {
			try{
				reader.close();
			} catch(IOException e) {
				throw new IOException("Eror: has problem in scanner.close() " + e.getMessage());
			}
		}
	}
}
