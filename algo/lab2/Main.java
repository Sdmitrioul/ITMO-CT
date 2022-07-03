import java.util.ArrayList;
import java.util.Arrays;

public class Main {
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";

    public static void main(String[] args) {
        String example = "##abde[fgh]ijk[mn]op";
        System.out.println(example.replaceAll("[\\[\\]#]", ""));

        //show("abaabaabababa");
        /*show("abababbbabaaabb");
        show("rioerrgfkerngs");
        show("ararrrraaarrrraaaa");
        show("abaacabaaab");
        show("aabaaaba");*/
    }

    private static void show(String s) {
        System.out.println();
        System.out.println(ANSI_RED + "STRING------------" + ANSI_RESET + s);
        System.out.println(ANSI_RED + "ZFunction---------" + ANSI_RESET + Arrays.toString(zFunction(s)));
        System.out.println(ANSI_RED + "ConvertedToZ------" + Arrays.toString(converterPrefixToZfun(prefixFunction(s))));
        System.out.println(ANSI_RED + "PrefixFunction----" + ANSI_RESET + Arrays.toString(prefixFunction(s)));
        System.out.println(ANSI_RED + "ConvertedToPREF---" + Arrays.toString(convertZfunToPrefixfun(zFunction(s))));
        System.out.println();
    }

    private static int[] converterPrefixToZfun(int[] p) {
        int length = p.length;
        int[] z = new  int[length];

        for(int i = 1; i < length; i++) {
            if (p[i] != 0) {
                z[i - p[i] + 1] = p[i];
            }
        }

        z[0] = 0;

        if(z[1] != 0) {
            for (int i = 1; i < z[1]; i++) {
                z[i + 1] = z[1] - i;
            }
        }

        for(int i = z[1] + 1, t = i; i < length - 1; i++) {
            t = i;
            if(z[i] != 0 && 0 == z[i + 1]) {
                for (int j = 1; j < z[i] && z[i + j] <= z[j]; j++) {
                    z[i + j] = Math.min(z[j], z[i] - j);
                    t = i + j;
                }
            }
            i = t;
        }

        return z;
    }

    private static int[] convertZfunToPrefixfun(int[] z) {
        int length = z.length;
        int[] pr = new int[length];
        for (int i = 1; i < length; i++) {
            if (z[i] != 0) {
                for (int j = z[i] - 1; j >= 0 /*&& pr[i + j] == 0*/; j--) {
                    pr[i + j] = Math.max(pr[i + j], j + 1);
                }
            }
        }
        return pr;
    }

    private static int[] zFunction(String s) {
        int length = s.length();
        int[] z = new int[length];
        for (int i = 1, l = 0, r = 0; i < length; ++i) {
            if (i <= r) {
                z[i] = Math.min(r - i + 1, z[i - l]);
            }
            while (i + z[i] < length && s.charAt(z[i]) == s.charAt(i + z[i])) {
                ++z[i];
            }
            if (i + z[i] - 1 > r) {
                l = i;
                r = i + z[i] - 1;
            }
        }
        return z;
    }

    private static int[] prefixFunction(String s) {
        int length = s.length();
        int[] pr = new int[length];
        for (int i = 1; i < length; ++i) {
            int j = pr[i-1];
            while (j > 0 && s.charAt(i) != s.charAt(j))
                j = pr[j-1];
            if (s.charAt(i) == s.charAt(j)) {
                ++j;
            }
            pr[i] = j;
        }
        return pr;
    }
}
