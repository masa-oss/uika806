/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * サロゲートペアも１文字として扱う文字
 *
 */
public final class SChar {

    static ConcurrentHashMap<Integer, SChar> MAP = new ConcurrentHashMap<>(257);
    
    private final int code;

    private SChar(int code) {

        if (code < 0 || code > 0x10FFFF) {
            throw new IllegalArgumentException("code=" + code);
        }
        
//        if (0xD8000 <= code && code <= 0xDFFF) {
        if (0xD800 <= code && code <= 0xDFFF) {
            throw new IllegalArgumentException("code=" + code);
        }
        

        this.code = code;
    }

    public static SChar valueOf(int codePoint) {
        
        SChar wk = MAP.get(codePoint);
        if (wk != null) {
            return wk;
        }
        
        SChar sc = new SChar(codePoint);
        MAP.putIfAbsent(codePoint, sc);
        
        return MAP.get(codePoint);
    }

    /**
     * @return the code
     */
    public int getCodepoint() {

        return code;
    }

    public int writeByte(byte[] toBuf, int siz) {

        byte[] from = new byte[4];

        if (0 <= code && code < 128) {
            from[0] = (byte) code;
            _write(from, 1, toBuf, siz);
            return 1;
        }

        int low1 = code & 0x0000003f;
        int low2 = code & 0x00000fc0;
        int low3 = code & 0x0003f000;
        int low4 = code & 0x00fc0000;

        final int TRAIL = 0x80;

        if (code > 65535) {
            from[0] = (byte) (0xf0 | (low4 >> 18));
            from[1] = (byte) (TRAIL | (low3 >> 12));
            from[2] = (byte) (TRAIL | (low2 >> 6));
            from[3] = (byte) (TRAIL | low1);
            _write(from, 4, toBuf, siz);
            return 4;
        }

        if (code >= 2048) {
            from[0] = (byte) (0xe0 | (low3 >> 12));
            from[1] = (byte) (TRAIL | (low2 >> 6));
            from[2] = (byte) (TRAIL | low1);
            _write(from, 3, toBuf, siz);
            return 3;
        }

        from[0] = (byte) (0xc0 | (low2 >> 6));
        from[1] = (byte) (TRAIL | low1);
        _write(from, 2, toBuf, siz);
        return 2;
    }

    private void _write(byte[] from, int fromSiz, byte[] to, int toSiz) {

        for (int i = 0; i < fromSiz; i++) {

            if (i >= toSiz) {
                throw new RuntimeException();
            }

            to[i] = from[i];

        }

    }
    
    
    public static int numberRegistered() {
        
        Set<Integer> keySet = MAP.keySet();
        return keySet.size();
    }

    public char[] toChar() {
        
        throw new UnsupportedOperationException();
/*        
        if (code > 65535) {
            char[] arr = new char[2];   // TODO
            arr[0] = (char) code;
            return arr;

        } else {
            char[] arr = new char[1];
            arr[0] = (char) code;
            return arr;
        }*/
    }

    public void appendTo(Appendable app) {

        throw new UnsupportedOperationException();

    }

    @Override
    public String toString() {

        return "SChar["
                + Integer.toHexString(code) + "]";
    }

    @Override
    public int hashCode() {
        return this.code;
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
        final SChar other = (SChar) obj;
        return this.code == other.code;
    }

}
