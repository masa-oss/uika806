package uika806.objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.err.LispException;
import uika806.pico.fn.Equal2Helper;

/**
 *
 * @author hemmi
 */
public final class SArray {
    
    private static final Logger LOG = LoggerFactory.getLogger(SArray.class);
    

    final Object[] arr;
    private final boolean mutable;

    public SArray(int siz, Object init) {
        this.arr = new Object[siz];
        for (int i = 0; i < siz; i++) {
            arr[i] = init;
        }
        this.mutable = true;
    }

    public SArray(Object[] init) {
        
        int siz = init.length;
        this.arr = new Object[siz];
        for (int i = 0; i < siz; i++) {
            arr[i] = init[i];
        }
        this.mutable = false;
    }

    public SArray(Object[] init, boolean mut) {
        
        int siz = init.length;
        this.arr = new Object[siz];
        for (int i = 0; i < siz; i++) {
            arr[i] = init[i];
        }
        this.mutable = mut;
    }
    
    private SArray(boolean mut, Object[] init) {
        
        this.arr = init;
        this.mutable = mut;
    }
    
    
    public SArray subvector(int start, int end) {
        
        int len = arr.length;
        if (start < 0 || len < start) throw new LispException("Bad index to subvector: " + start);
        if (end < start || len < end) throw new LispException("Bad index to subvector: " + end);
        
        int newLen = end -start;
        
        Object[] narr = new Object[newLen];
        for (int i = 0; i < newLen; i++) {
            narr[i] = arr[i + start];
        }
        return new SArray(true, narr);
    }
    
    
    public static SArray append(SArray s1, SArray s2, boolean mutable) {    

        final int len1 = s1.length();
        final int len2 = s2.length();
        Object[] arr = new Object[  len1 + len2     ];
        
        
        for (int i = 0; i < len1; i++) {
            arr[i] = s1.getNth(i);
        }
        for (int j = 0; j < len2; j++) {

            arr[len1 + j] = s2.getNth(j);
        }        
        return new SArray(arr, mutable);
    }
    
    
    public void copyFrom(final SArray ss, int at, int start, int end) {
        
        int len = arr.length;
        if (at < 0 || len <= at) throw new IllegalArgumentException("string-copy!, at");

        int len3 = ss.length();
        if (start < 0 || len3 <= start) throw new IllegalArgumentException("string-copy!, start");
        if (end < 0 || len3 < end) throw new IllegalArgumentException("string-copy!, end");
        if (end < start) throw new IllegalArgumentException("string-copy!, start, end");
        
        
        Object[] target =   arr;
        if (ss == this) {
            target = new Object[ arr.length  ];
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

    public Object getNth(int nth) {
        return arr[nth];
    }

    public Object setNth(int nth, Object newValue) {

        if (mutable) {

            Object old = arr[nth];
            arr[nth] = newValue;
            return old; // 仕様では、未定義
        } else {
            throw new LispException("This vector is immutable");
        }
    }
    
    

    @Override
    public String toString() {
        return "SArray[" + arr.length + "]";
    }
    
    static Class<?> CLAZZ = Equal2Helper.class;

    /**
     * <i>PrinterSmall</i> is used to process <i>Equal2Helper</i>.
     * 
     * For TreeMap in <i>Equal2Helper</i> to work correctly,
     * this class must not implement <i>hashCode </i>  and <i>equals </i>  .
     * 
     * <br>
     * Equal2Helper#checkCircular メソッドのため、
     * この hashCode() は、このオブジェクトのメモリ上のアドレスを返すものとする。
     * 
     * <b>通常のJavaのhashCodeの実装ルールには従わない。</b>
     * 
     * @return 
     */
    @Override
    public int hashCode() {
        
        return super.hashCode();
    }

    
    @Override
    public boolean equals(Object obj) {
        
        LOG.info("equals()");
        
        if (this == obj) {
            return true;
        }
        return false;
    }

    
}
