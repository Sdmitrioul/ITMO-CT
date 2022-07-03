package commonString;

import java.awt.desktop.PreferencesEvent;
import java.util.Arrays;
import java.util.Objects;

public class MyString {
    private final int length;
    private Object[] myString;


    public MyString(Object[] myString) {
        this.myString = myString;
        this.length = myString.length;
    }

    public MyString(Object[] src, int pos, int len) {
        this.length = len;
        myString = new Object[len];
        System.arraycopy(src, pos, this.myString, 0, len);
    }

    public Object at(int i) {
        assert i > this.length;
        assert i < 0;

        if (i < 0) {
            throw new StringIndexOutOfBoundsException(i);
        }
        if (i >= this.length) {
            throw new StringIndexOutOfBoundsException(i);
        }

        return myString[i];
    }

    public MyString add(Object c) {
        assert c != null;

        Object[] nar = new Object[this.length + 1];
        System.arraycopy(this.myString, 0, nar, 0, this.length);
        nar[this.length] = c;

        return new MyString(nar);
    }

    public MyString insert(int i, Object c) {
        assert c != null;

        if (i < 0) {
            throw new StringIndexOutOfBoundsException(i);
        }
        if (i > this.length) {
            throw new StringIndexOutOfBoundsException(i);
        }

        Object[] nar = new Object[this.length + 1];
        System.arraycopy(this.myString, 0, nar, 0, i);
        nar[i] = c;
        System.arraycopy(this.myString, i, nar, i + 1, this.length - i);
        return new MyString(nar);
    }

    public int length() {
        return this.length;
    }

    public MyString substring(int i) {
        if (i < 0) {
            throw new StringIndexOutOfBoundsException(i);
        }
        if (i >= this.length) {
            throw new StringIndexOutOfBoundsException(i);
        }

        int len = this.length - i;

        return new MyString(this.myString, i, len) ;
    }

    public MyString substring(int i, int j) {
        if (i < 0) {
            throw new StringIndexOutOfBoundsException(i);
        }
        if (j > this.length) {
            throw new StringIndexOutOfBoundsException(j);
        }

        int len = j - i;
        i = Math.min(i, j);

        if (len < 0) {
            throw new StringIndexOutOfBoundsException(len);
        }

        return new MyString(this.myString, i, len);
    }

    public MyString concat(MyString s) {
        assert s != null;

        Object[] na = new Object[this.length + s.length];
        System.arraycopy(this.myString, 0, na, 0, this.length);
        System.arraycopy(s.myString, 0, na, this.length, s.length);

        return new MyString(na);
    }

    public MyString beginsWith(MyString s) {
        assert s != null;

        //if (s.length > this.length) return null;

        for (int i = 0; i <= this.length - s.length; i++) {
            boolean c = true;
            for (int j = 0; j < s.length; j++) {
                if (!this.myString[i + j].equals(s.myString[j])) {
                    c = false;
                    break;
                }
            }
            if (c) {
                return new MyString(this.myString, i, this.length - i);
            }
        }

        return null;
    }

    public MyString endsWith(MyString s) {
        assert s != null;

        if (s.length > this.length) return null;

        for (int i = 0; i <= this.length - s.length; i++) {
            boolean c = true;
            for (int j = 0; j < s.length; j++) {
                if (!this.myString[i + j].equals(s.myString[j])) {
                    c = false;
                    break;
                }
            }
            if (c) {
                return new MyString(this.myString, 0, i + s.length);
            }
        }

        return null;
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        MyString myString1 = (MyString) o;
        return length == myString1.length &&
                Arrays.equals(myString, myString1.myString);
    }

    public int hashCode() {
        int result = 23;
        result = result * 31 + Objects.hash(length);
        result = 31 * result + Arrays.hashCode(myString);
        return result;
    }
}
