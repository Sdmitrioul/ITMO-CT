package search;

public class BinarySearchMissing {
    public static void main(String[] args) {
        int[] massive = new int[args.length - 1];
        int x = Integer.parseInt(args[0]);
        for (int i = 1; i < args.length; i++) {
            massive[i - 1] = Integer.parseInt(args[i]);
        }
        System.out.println(beginBinarySearch(massive, x));
    }

    //Pre: array "massive" is sorted by non-ascending order
    // INV: array massive is array of integers && x is integer
    private static int beginBinarySearch(int[] massive, int x) {
        if (massive.length == 0)
            // massive.length == 0 => insertion point == 0 => (-(insertion point) - 1)  == -1 < 0
            return -1;
        if (massive[massive.length - 1] > x){
            // x smaller than the smallest number in massive => position == -(insertion point) - 1
            return -massive.length - 1;
        } else if (massive[0] < x) {
            // x bigger than the biggest number in massive => position == - 1
            return -1;
        } else {
            // INV: massive[beginIndex] >= x >= massive[endIndex]  && beginIndex <= lendIndex && 0 <= beginIndex(endIndex) < massive.length
            return recursiveBinarySearch(massive, 0, massive.length - 1, x);
            //return iterativeBinarySearch(massive, x);
        }
    }
    // Post: (massive[endIndex] == x || massive[endIndex] != x && return (-insertion_point - 1)
    // array "massive" not changed


    //Pre: array "massive" is sorted by non-ascending order
    // INV: massive[beginIndex] >= x >= massive[endIndex]  && beginIndex <= endIndex < masdsive.length && 0 <= beginIndex(endIndex) < massive.length
    private static int recursiveBinarySearch(int[] massive, int beginIndex, int endIndex, int x) {
        if (beginIndex == endIndex) {
            if (massive[endIndex] == x) {
                // x == massive[endIndex] && INV
                // x == a[endIndex] && (a[endIndex - 1] > x || endIndex == 0))
                return endIndex;
            } else {
                // x > massive[endIndex] && INV
                // x > massive[endIndex] && (a[endIndex - 1] > x || endIndex == 0))
                // insertrion point = endIndex
                // -1 - endIndex == (-(insertion point) - 1) < 0
                return -endIndex - 1;
            }
        }

        // INV && endIndex != beginIndex
        // endIndex > beginIndex && INV
        int middle = beginIndex + (endIndex - beginIndex) / 2;
        // middle > beginIndex, middle < endIndex && INV

        if (massive[middle] > x) {
            // massive[middle] > x && INV && middle > beginIndex && middle < endIndex
            // massive[middle] > x >= massive[endIndex] && endIndex > middle && middle > beginIndex (that is why process will be ended)
            return recursiveBinarySearch(massive, middle + 1, endIndex, x);
        } else {
            // x >= massive[middle] && INV && middle > beginIndex && middle < endIndex
            // massive[beginIndex] > x >= massive[middle] && middle > beginIndex && middle < endIndex (that is why process will be ended)
            return recursiveBinarySearch(massive, beginIndex, middle, x);
        }
    }
    // Post: (((massive[endIndex] == x && return endIndex) || massive[endIndex] != x && return (-insertion_point - 1) && endIndex == beginIndex)
    // || (massive[middle] > x && beginIndex = middle + 1) || (massive[middle] <= x && endIndex = middle)
    // array "massive" not changed

    //Pre: array "massive" is sorted by non-ascending order
    // INV: massive[0] >= x >= massive[massive.length - 1]
    private static int iterativeBinarySearch(int[] massive, int x) {
        int beginIndex = 0;
        int endIndex = massive.length - 1;

        // INV: massive[beginIndex] >= x >= massive[endIndex]  && beginIndex <= lendIndex && 0 <= beginIndex(endIndex) < massive.length
        while (beginIndex < endIndex) {
            // INV && endIndex != beginIndex
            // endIndex > beginIndex && INV
            int middle = beginIndex + (endIndex - beginIndex) / 2;
            // middle > beginIndex, middle < endIndex && INV

            if (massive[middle] > x) {
                // massive[middle] > x && INV && middle > beginIndex && middle < endIndex
                // massive[middle] > x >= massive[endIndex] && endIndex > middle && middle > beginIndex (that is why process will be ended)
                beginIndex = middle + 1;
            } else {
                // x >= massive[middle] && INV && middle > beginIndex && middle < endIndex
                // massive[beginIndex] > x >= massive[middle] && middle > beginIndex && middle < endIndex (that is why process will be ended)
                endIndex = middle;
            }
        }
        // Post: (((massive[endIndex] == x && return endIndex) || massive[endIndex] != x && return (-insertion_point - 1) && endIndex == beginIndex)
        // || (massive[middle] > x && beginIndex = middle + 1) || (massive[middle] <= x && endIndex = middle)

        if (massive[endIndex] == x) {
            // x == massive[endIndex] && INV
            // x == a[endIndex] && (a[endIndex - 1] > x || endIndex == 0))
            return endIndex;
        } else {
            // x > massive[endIndex] && INV
            // x > massive[endIndex] && (a[endIndex - 1] > x || endIndex == 0))
            // insertrion point = endIndex
            // -1 - endIndex == (-(insertion point) - 1) < 0
            return -endIndex - 1;
        }
    }
    // Post: (massive[endIndex] == x || massive[endIndex] != x && return (-insertion_point - 1)
    // array "massive" not changed
}
