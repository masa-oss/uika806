/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import java.io.UnsupportedEncodingException;
import java.util.List;
import uika806.err.LispException;

/**
 *
 * @author hemmi
 */
public final class U8Array {

    private final byte[] arr;
    private final boolean mutable;
    

    public U8Array(int siz, byte init) {
        this.arr = new byte[siz];
        for (int i = 0; i < siz; i++) {
            arr[i] = init;
        }
        this.mutable = true;
    }

    public U8Array(byte[] init) {
        
        int siz = init.length;
        this.arr = new byte[siz];
        for (int i = 0; i < siz; i++) {
            arr[i] = init[i];
        }
        this.mutable = false;
    }

    public U8Array(byte[] init, boolean mut) {
        
        int siz = init.length;
        this.arr = new byte[siz];
        for (int i = 0; i < siz; i++) {
            arr[i] = init[i];
        }
        this.mutable = mut;
    }
    
    private U8Array(boolean mut, byte[] init) {
        
        this.arr = init;
        this.mutable = mut;
    }
    
    public U8Array(List<Integer> init, boolean mut) {
        
        int siz = init.size();
        this.arr = new byte[siz];
        for (int i = 0; i < siz; i++) {
            
            int code = init.get(i);
            if (code < 0) {
                code = 0;
            } else if (code > 255) {
                code = 255;
            }
            
            arr[i] = (byte) code;
        }
        this.mutable = mut;
    }
    
    
    
    public String utf8ToAtring() throws UnsupportedEncodingException {
        
        return new String(arr, "UTF-8");
    }
    
    public String utf8ToAtring(int start, int end) throws UnsupportedEncodingException {
        
        int len = arr.length;
        if (start < 0 || len < start) throw new IllegalArgumentException("utf8ToAtring");
        if (end < 0 || len < end) throw new IllegalArgumentException("utf8ToAtring");
        if (!(start <= end)) throw new IllegalArgumentException("utf8ToAtring");

        byte[] buf = new byte[end - start];
        int j = 0;
        for (int i = start; i < end; i++) {
            buf[j++] = arr[i];
        }
        
        return new String(buf, "UTF-8");
    }
    
    public U8Array subvector(int start, int end) {
        
        int len = arr.length;
        if (start < 0 || len < start) throw new LispException("Bad index to subvector: " + start);
        if (end < start || len < end) throw new LispException("Bad index to subvector: " + end);
        
        int newLen = end -start;
        
        byte[] narr = new byte[newLen];
        for (int i = 0; i < newLen; i++) {
            narr[i] = arr[i + start];
        }
        return new U8Array(true, narr);
    }
    
    public static U8Array append(U8Array s1, U8Array s2, boolean mutable) {    

        final int len1 = s1.length();
        final int len2 = s2.length();
        byte[] arr = new byte[  len1 + len2     ];
        
        
        for (int i = 0; i < len1; i++) {
            arr[i] = s1.getNth(i);
        }
        for (int j = 0; j < len2; j++) {

            arr[len1 + j] = s2.getNth(j);
        }        
        return new U8Array(arr, mutable);
    }
    
    
    
    
    public void copyFrom(final U8Array ss, int at, int start, int end) {
        
        int len = arr.length;
        if (at < 0 || len <= at) throw new IllegalArgumentException("bytevector-copy!, at");

        int len3 = ss.length();
        if (start < 0 || len3 <= start) throw new IllegalArgumentException("bytevector-copy!, start");
        if (end < 0 || len3 < end) throw new IllegalArgumentException("bytevector-copy!, end");
        if (end < start) throw new IllegalArgumentException("bytevector-copy!, start, end");
        
        
        byte[] target =   arr;
        if (ss == this) {
            target = new byte[ arr.length  ];
            int nn = arr.length;
            for (int i = 0; i < nn; i++) {
                target[i] = arr[i];
            }
        }

        for (int i = start,  k = at; i < end; i++) {
            target[k] = ss.getNth(i);
            k++;
        }
        
        

        if (ss == this) {
            int len2 = target.length;
            for (int j = 0; j < len2; j++) {
                arr[j] = target[j];
            }
        }

    }
    
    
    
    public int length() {
        return arr.length;
    }

    private byte getNth(int nth) {
        return arr[nth];
    }
    
    public int getNthAsInt(int nth) {
        return (arr[nth]) & 255;
    }

    public byte setNth(int nth, long code) {

        if (mutable) {
            if (code < 0 || 255 < code) throw new IllegalArgumentException("Arguments is not in the range [0, 255]");

            byte old = arr[nth];
            arr[nth] = (byte) code;
            return old; // 仕様では、未定義
        } else {
            throw new LispException("This vector is immutable");
        }
    }
    
    

    @Override
    public String toString() {
        return "U8Array[" + arr.length + "]";
    }
    
    
}
