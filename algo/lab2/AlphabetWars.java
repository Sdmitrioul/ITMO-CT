public class AlphabetWars {
    public static void main(String[] args) {
        System.out.println(alphabetWar("#abde[fgh]i#jk[mn]op"));
    }
    public static String alphabetWar(String battlefield) {
        int[][]shelters = new int[battlefield.length()][3];

        int last = -1;
        int count = 0;

        for (int i = 0; i < battlefield.length(); i++) {
            switch (battlefield.charAt(i)) {
                case '#':
                    count++;
                    if (last != -1) {
                        shelters[last][2]++;
                    }
                    shelters[last + 1][2]++;
                    break;

                case '[':
                    last++;
                    shelters[last][0] = i + 1;
                    char c = battlefield.charAt(i);
                    while (c != ']') {
                        c = battlefield.charAt(++i);
                    }
                    shelters[last][1] = i;
                    break;

                default:
                    //No operations
            }
        }

        if (count == 0) {
            battlefield = battlefield.replaceAll("[\\[\\]# ]", "");
        } else {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; shelters[i][1] != 0; i++) {
                if (shelters[i][2] < 2) {
                    sb.append(battlefield.substring(shelters[i][0], shelters[i][1]));
                }
            }
            battlefield = sb.toString();
        }

        return battlefield;
    }
}
