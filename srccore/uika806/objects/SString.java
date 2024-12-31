/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import uika806.err.LispException;
import java.util.Arrays;
import java.util.List;

/**
 *
 * Move uika806.pico.objects to uika806.small.objects
 */
public final class SString {

    final SChar[] arr;
    private final boolean mutable;

    /**
     * Make a mutable string.
     *
     * @param len
     * @param ch
     */
    public SString(int len, int ch) {
        
        SChar sc = SChar.valueOf(ch);
        this.arr = new SChar[len];
        for (int i = 0; i < len; i++) {
            arr[i] = sc;
        }
        mutable = true;
    }

    private SString(int[] str, boolean writable) {
        
        int len = str.length;
        this.arr = new SChar[len];
        for (int i = 0; i < len; i++) {
            arr[i] = SChar.valueOf(str[i]);
        }
        mutable = writable;
    }

    private SString(SChar[] str, boolean writable) {
        
        arr = str;
        mutable = writable;
    }


    /**
     * Make a immutable string.
     *
     * @param list
     * @return
     */
    public static SString fromList(List<Integer> list) {

        int len = list.size();
        int[] arr = new int[len];
        for (int i = 0; i < len; i++) {
            arr[i] = list.get(i);
        }
        return new SString(arr, false);
    }
    
    public static SString fromList(List<Integer> list, boolean mutable) {

        int len = list.size();
        int[] arr = new int[len];
        for (int i = 0; i < len; i++) {
            arr[i] = list.get(i);
        }
        return new SString(arr, mutable);
    }
    
    public static SString fromString(String text) {    

        int cpLen = codePointLength(text);
        
        int[] arr = new int[cpLen];
        
        int len = text.length();
        for (int i = 0; i < len; i++) {
            
            arr[i] = text.codePointAt(i);
            
            char c = text.charAt(i);
            if (Character.isSurrogate(c)) {
                i++;
            }
        }
        return new SString(arr, false);
    }    

    
    public void copyFrom(final SString ss, int at, int start, int end) {
        
        int len = arr.length;
        if (at < 0 || len <= at) throw new IllegalArgumentException("string-copy!, at");

        int len3 = ss.length();
        if (start < 0 || len3 <= start) throw new IllegalArgumentException("string-copy!, start");
        if (end < 0 || len3 < end) throw new IllegalArgumentException("string-copy!, end");
        if (end < start) throw new IllegalArgumentException("string-copy!, start, end");
        
        
        SChar[] target =   arr;
        if (ss == this) {
            target = new SChar[ arr.length  ];
            int nn = arr.length;
            for (int i = 0; i < nn; i++) {
                target[i] = arr[i];
            }
        }

        for (int i = start,  k = at; i < end; i++) {
            target[k] = ss.getNthChar(i);
            k++;
        }
        
        

        if (ss == this) {
            int len2 = target.length;
            for (int j = 0; j < len2; j++) {
                arr[j] = target[j];
            }
        }

    }
    
    
    public static SString append(SString s1, SString s2, boolean mutable) {    

        final int len1 = s1.length();
        final int len2 = s2.length();
        SChar[] arr = new SChar[  len1 + len2     ];
        
        
        for (int i = 0; i < len1; i++) {
            arr[i] = s1.getNthChar(i);
        }
        for (int j = 0; j < len2; j++) {

            arr[len1 + j] = s2.getNthChar(j);
        }        
        return new SString(arr, mutable);
    }
    
    
    
    static int codePointLength(String text) {
        
        int result = 0;
        int len = text.length();
        for (int i = 0; i < len; i++) {
            char c = text.charAt(i);
            
            if (Character.isSurrogate(c)) {
                i++;
            }
            result++;
        }
        return result;
    }

    
    
    public SString substring(int start, int end) {
        
        int len = arr.length;
        if (start < 0 || len < start) throw new LispException("Bad index to substring: " + start);
        if (end < start || len < end) throw new LispException("Bad index to substring: " + end);
        
        int newLen = end -start;
        SChar[] narr = new SChar[newLen];
        for (int i = 0; i < newLen; i++) {
            narr[i] = arr[i + start];
        }
        return new SString(narr, true);
    }
    
    

    public int length() {
        return arr.length;
    }

    public int getNth(int nth) {
        return arr[nth].getCodepoint();
    }

    
    public SChar getNthChar(int nth) {
        return arr[nth];
    }
    
    public int setNthChar(int nth, SChar sc) {

        if (mutable) {

            int old = arr[nth].getCodepoint();
            arr[nth] = sc;
            return old; // 仕様では、未定義
        } else {
            throw new LispException("This string is immutable");
        }
    }
    
    
    
    public int setNth(int nth, int code) {

        if (mutable) {

            int old = arr[nth].getCodepoint();
            arr[nth] = SChar.valueOf(code) ;
            return old; // 仕様では、未定義
        } else {
            throw new LispException("This string is immutable");
        }
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        int len = arr.length;
        for (int i = 0; i < len; i++) {
            int code = arr[i].getCodepoint();
            sb.appendCodePoint(code);
        }

        return sb.toString();
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 59 * hash + Arrays.hashCode(this.arr);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final SString other = (SString) obj;
        return Arrays.equals(this.arr, other.arr);
    }

}
