/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */

package uika806.fn011.reader;

import uika806.reader.LispReaderEx;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.LoggerFactory;

import uika806.err.ReaderException;
import uika806.objects.SChar;
import uika806.objects.Ratio;
import uika806.objects.Undef;


public class SharpMacroReader {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(SharpMacroReader.class);

    public Object sharp(Token tk, boolean eofError, Object eofval, LispReaderEx lispReader) {

        List<Integer> str = tk.str;

        switch (str.get(0)) {

            case 0x5c:  // #\
                return toSChar(str);

            case 0x66:  // #f
                return Boolean.FALSE;

            case 0x74:  // #t
                return Boolean.TRUE;

            case 0x62: // #b  2進数
            {
                String s2 = lispReader.listToString(str, 1);

                return Long.parseLong(s2, 2);
            }
            case 0x6f: // #o   8進数
            {
                String s8 = lispReader.listToString(str, 1);

                return Long.parseLong(s8, 8);
            }

            case 0x78: // #x   16進数
            {
                String s8 = lispReader.listToString(str, 1);

                return Long.parseLong(s8, 16);
            }

            case 0x64: // #d   10進数
            {
                String s8 = lispReader.listToString(str, 1);

                return Long.parseLong(s8, 10);
            }
            case 0x65: // #e   正確数
            {
                String s9 = lispReader.listToString(str, 1);
                return readExact(s9);
            }
            case 0x69: // #i   不正確数
            {
                String s10 = lispReader.listToString(str, 1);
                ArrayList<Integer> newList = new ArrayList<>();
                s10.codePoints().forEach((i) -> newList.add(i));
                Token newT = new Token('N', newList, lispReader.tokenizer.in.getLineNum());
                Object o = lispReader.toNumber(newT);
                Object result = null;
                try {
                    result = readInexact(o);
                } catch (Exception ex) {
                    throw new ReaderException("Syntax error near #i" ,
                            lispReader.tokenizer.in.getLineNum(),
                            lispReader.tokenizer.in.getNthChar());

                }
                return result;
            }
            case 0x21: //    '!'
            {
                if (strcmp(str, fold_case)) {
                    
                    lispReader.foldStatus = "fold-case";
                    LOG.info("     change to  'fold-case'");
                    return null;
                }
                if (strcmp(str, no_fold_case)) {
                    
                    lispReader.foldStatus = "no-fold-case";
                    LOG.info("     change to  'no-fold-case'");
                    return null;
                }
                
            }
            case 0x3C: // #<Undef>
            {
                if (strcmp(str, undef)) {
                    return Undef.Undefined;
                }
            }
        }

        throw new ReaderException("Unsupported char=" + str.get(0),
                lispReader.tokenizer.in.getLineNum(),
                lispReader.tokenizer.in.getNthChar());
    }
    
    
    static int[] fold_case = new int[]{
        '!', 'f', 'o', 'l', 'd', '-', 'c', 'a', 's', 'e'
    };
    
    static int[] no_fold_case = new int[]{
        '!', 'n', 'o', '-',   'f', 'o', 'l', 'd', '-', 'c', 'a', 's', 'e'
    };
    
    static int[] undef = new int[]{
        '<',   'U', 'n', 'd', 'e', 'f', '>'
    };
    
    boolean strcmp(List<Integer> str , int[] compare) {
        
        int len1 = str.size();
        int len2 = compare.length;
        if (len1 != len2) {
            return false;
        }
        for (int i = 0; i < len1; i++) {
            Integer get = str.get(i);
            if (compare[i] != get ) {
                return false;
            }
        }
        
        return true;
    }
    
    

    Object readInexact(Object o) throws Exception {
        if (o instanceof Ratio) {
            Ratio ra = (Ratio) o;
            return ra.doubleValue();
        }
        if (o instanceof Long) {
            return ((Long) o).doubleValue();
        }
        if (o instanceof Double) {
            return o;
        }
        throw new IllegalStateException();
    }

    Object readExact(String str) {

        //  LOG.info("str={}"  , str  );
        int k = str.indexOf("e");
        if (k >= 0) {

            String left = str.substring(0, k);
            String right = str.substring(k + 1);

            //     LOG.info("left={}, right={}" , left, right );
            return readExact2(left, right);
        }

        return Ratio.parseRatio(str);
    }

    Object readExact2(String left, String right) {

        long ll = Long.parseLong(left);
        int rr = Integer.parseInt(right);

        for (int i = 0; i < rr; i++) {
            ll = Math.multiplyExact(ll, 10);
        }
        return ll;
    }

    Object[] dict = {"alarm", 7,
        "backspace", 8,
        "delete", 127,
        "escape", 0x1b,
        "newline", 10,
        "null", 0,
        "return", 13,
        "space", 32,
        "tab", 9
    };

    boolean match(List<Integer> str, String word) {

        int n = word.length();
        for (int j = 0; j < n; n++) {
            char c = word.charAt(j);
            if (str.get(j + 1) != c) {
                return false;
            }
        }
        return true;
    }

    int find(List<Integer> str) {

        int len = dict.length / 2;
        for (int i = 0; i < len; i++) {
            String word = (String) dict[2 * i];
            if (word.length() != str.size() - 1) {
                continue;
            }
            if (match(str, word)) {
                Integer it = (Integer) dict[2 * i + 1];
                return it;
            } else {
                continue;
            }
        }
        return -1; // not found
    }

    SChar toSChar(List<Integer> str) {

        int ma = find(str);
        if (ma >= 0) {
            return SChar.valueOf(ma);
        }
        int n = str.get(1);
        if (n == 120) {
            // x
            int len = str.size();
            StringBuilder sb = new StringBuilder();

            int idx = 2;
            // add
            if (idx >= len) {

                return SChar.valueOf(120); // x
            }

            int d = str.get(idx);
            while (Tokenizer.isHexChar(d)) {
                sb.appendCodePoint(d);
                idx++;
                if (idx >= len) {
                    break;
                }
                d = str.get(idx);
            }
            int nt = Integer.parseInt(sb.toString(), 16);

            return SChar.valueOf(nt);

        }

        return SChar.valueOf(str.get(1));
    }

}
