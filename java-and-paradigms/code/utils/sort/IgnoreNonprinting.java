import java.util.*;

public class IgnoreNonprinting implements Sorting {
    private ArrayList<Map.Entry<String, String>> list;
    private Map<String, String> map;

    public IgnoreNonprinting(ArrayList<String> list) {
        map = new LinkedHashMap<>();
        for (String iterator : list) {
            StringBuilder stringBuilder = new StringBuilder();
            for (int i = 0; i < iterator.length(); i++) {
                if (Character.isDigit(iterator.charAt(i)) || Character.isLetter(iterator.charAt(i))) {
                    stringBuilder.append(iterator.charAt(i));
                }
            }
            map.put(iterator, stringBuilder.toString());
        }
    }

    @Override
    public void sort() {
        list = new ArrayList<>(map.entrySet());
        Collections.sort(list, new Comparator<Map.Entry<String, String>>() {
            @Override
            public int compare(Map.Entry<String, String> o1, Map.Entry<String, String> o2) {
                return o1.getValue().compareTo(o2.getValue());
            }
        });
    }

    @Override
    public void print() {
        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i).getKey());
        }
    }

    @Override
    public ArrayList<String> printInFile() {
        ArrayList<String> ret = new ArrayList<>();
        for (int i = 0; i < list.size(); i++) {
            ret.add(list.get(i).getKey());
        }
        return ret;
    }
}
