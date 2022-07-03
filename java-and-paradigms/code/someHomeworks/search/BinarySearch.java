package search;

public class BinarySearch {
    public static void main(String[] args) {
        int[] massive = new int[args.length - 1];
        int x = Integer.valueOf(args[0]);
        for (int i = 1; i < args.length; i++) {
            massive[i - 1] = Integer.valueOf(args[i]);
        }
        if (massive.length - 1 >= 0) {
            if (massive[massive.length - 1] > x){
                System.out.println(massive.length);
            } else if (massive[0] < x){
                System.out.println("0");
            } else {
                System.out.println(/*recursiveBinarySearch(massive, 0, massive.length - 1, x)*/ iterativeBinarySearch(massive, x));
            }
        } else {
            System.out.println("0");
        }
    }

    private static int recursiveBinarySearch(int[] massive, int beginIndex, int endIndex, int x) {
        int middle = beginIndex + (endIndex - beginIndex) / 2;
        if (beginIndex >= endIndex) {
            if (massive[endIndex] == x) {
                return endIndex;
            } else {
                return beginIndex;
            }
        } else if (massive[middle] > x) {
            return recursiveBinarySearch(massive, middle + 1, endIndex, x);
        } else {
            return recursiveBinarySearch(massive, beginIndex, middle, x);
        }
    }

    private static int iterativeBinarySearch(int[] massive, int x) {
        int beginIndex = 0;
        int endIndex = massive.length - 1;
        while (beginIndex < endIndex) {
            int middle = beginIndex + (endIndex - beginIndex) / 2;
            if (massive[middle] > x) {
                beginIndex = middle + 1;
            } else {
                endIndex = middle;
            }
        }
        if (massive[endIndex] == x) {
            return endIndex;
        } else {
            return beginIndex;
        }
    }
}
