import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

import static java.lang.Math.*;

public class F {
	private static final String READING_PROBLEM = "Exception, while reading: ";
	private static final String WRITING_PROBLEM = "Exception, while writing in buffer: ";
	
	private static final ComplexNumber TWO = new ComplexNumber(2, 0);
	
	public static void main(String[] args) {
		new F().run();
	}
	
	public int n;
	
	public void run() {
		try (final Scanner scanner = new Scanner(System.in)) {
			long[][] input = input(scanner);
			long[] ans = calculate(input);
			output(ans);
		} catch (IOException ex) {
			System.err.println(READING_PROBLEM);
		}
	}
	
	private long[] calculate(final long[][] input) {
		return multiply(input[0], input[1]);
	}
	
	public long[] multiply(final long[] left, final long[] right) {
		ComplexNumber[] nLeft = ComplexNumber.getArray(left);
		ComplexNumber[] nRight = ComplexNumber.getArray(right);
		
		fft(nLeft, false);
		fft(nRight, false);
		
		int arrayLength = nLeft.length;
		
		for (int i = 0; i < arrayLength; i++) {
			nLeft[i] = ComplexNumber.multiply(nLeft[i], nRight[i]);
		}
		
		fft(nLeft, true);
		
		long[] result = new long[arrayLength];
		
		for (int i = 0; i < arrayLength; i++) {
			result[i] = (long) (nLeft[i].getRe() + 0.5);
		}
		
		int counter = 0;
		
		for (int i = arrayLength - 1; i >= 0; i--) {
			if (result[i] != 0) {
				break;
			}
			counter++;
		}
		
		return Arrays.copyOfRange(result, 0, arrayLength - counter);
	}
	
	public void fft(ComplexNumber[] array, boolean isInvert) {
		final int arrayLength = array.length;
		if (arrayLength == 1) {
			return;
		}
		
		ComplexNumber[] left = new ComplexNumber[arrayLength / 2];
		ComplexNumber[] right = new ComplexNumber[arrayLength / 2];
		
		for (int i = 0, j = 0; i < arrayLength; i += 2, j++) {
			left[j] = array[i];
			right[j] = array[i + 1];
		}
		
		fft(left, isInvert);
		fft(right, isInvert);
		
		double ang = 2 * PI / arrayLength * (isInvert ? -1 : 1);
		ComplexNumber w = new ComplexNumber(1, 0);
		ComplexNumber wn = new ComplexNumber(cos(ang), sin(ang));
		for (int i = 0; i < arrayLength / 2; i++) {
			ComplexNumber tmp = ComplexNumber.multiply(w, right[i]);
			array[i] = ComplexNumber.sum(left[i], tmp);
			array[i + arrayLength / 2] = ComplexNumber.subtract(left[i], tmp);
			if (isInvert) {
				array[i] = ComplexNumber.divide(array[i], TWO);
				array[i + arrayLength / 2] = ComplexNumber.divide(array[i + arrayLength / 2], TWO);
			}
			w = ComplexNumber.multiply(w, wn);
		}
	}
	
	public long[][] input(final Scanner scanner) throws IOException {
		n = 0;
		if (scanner.hasLine()) {
			n = scanner.getNextInt() + 1;
		}
		
		int temp = 1;
		while (temp < 2 * n + 1) {
			temp *= 2;
		}
		
		long[][] input = new long[2][temp];
		
		for (int i = 0; i < 2; i++) {
			if (scanner.hasLine()) {
				for (int j = 0; j < n; j++) {
					input[i][j] = scanner.getNextInt();
				}
			}
		}
		
		return input;
	}
	
	public void output(final long[] ans) {
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
			writer.write(Arrays.stream(ans).mapToObj(Long::toString).collect(Collectors.joining(" ")));
		} catch (IOException ex) {
			System.err.println(WRITING_PROBLEM + ex.getMessage());
		}
	}
	
	private static class ComplexNumber {
		private final double re;
		private final double im;
		
		public ComplexNumber(double re, double im) {
			this.re = re;
			this.im = im;
		}
		
		public double getRe() {
			return re;
		}
		
		public double getIm() {
			return im;
		}
		
		private double getModule() {
			return sqrt(this.re * this.re + this.im * this.im);
		}
		
		public static ComplexNumber sum(ComplexNumber num1, ComplexNumber num2) {
			return new ComplexNumber(num1.getRe() + num2.getRe(), num1.getIm() + num2.getIm());
		}
		
		public static ComplexNumber multiply(ComplexNumber num1, ComplexNumber num2) {
			return new ComplexNumber(num1.getRe() * num2.getRe() - num1.getIm() * num2.getIm(), num1.getRe() * num2.getIm() + num1.getIm() * num2.getRe());
		}
		
		public static ComplexNumber subtract(ComplexNumber num1, ComplexNumber num2) {
			return new ComplexNumber(num1.getRe() - num2.getRe(), num1.getIm() - num2.getIm());
		}
		
		public static ComplexNumber divide(ComplexNumber num1, ComplexNumber num2) {
			ComplexNumber temp = new ComplexNumber(num2.getRe(), (-1) * num2.getIm());
			temp = ComplexNumber.multiply(num1, temp);
			double denominator = num2.getRe() * num2.getRe() + num2.getIm() * num2.getIm();
			return new ComplexNumber(temp.getRe() / denominator, temp.getIm() / denominator);
		}
		
		public static ComplexNumber[] getArray(long[] arr) {
			ComplexNumber[] array = new ComplexNumber[arr.length];
			for (int i = 0; i < arr.length; i++) {
				array[i] = new ComplexNumber(arr[i], 0);
			}
			return array;
		}
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
