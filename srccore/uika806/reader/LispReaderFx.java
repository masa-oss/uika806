/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.reader;

import org.slf4j.LoggerFactory;

import uika806.objects.Cell;
import uika806.fn011.reader.Token;
import uika806.fn011.reader.Tokenizer;

import uika806.objects.SString;
import uika806.objects.SArray;
import uika806.objects.U8Array;

/**
 */
public class LispReaderFx extends LispReaderEx {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(LispReaderFx.class);

    public LispReaderFx(Tokenizer instance) {
        super(instance);
    }

    @Override
    public Object toSString(Token tk, boolean eofError, Object eofval) {

        // test17.scmのバグ調査
        /*
        List<Integer> str = tk.str;
        int len = str.size();

        for (int i = 0; i < len; i++) {
            LOG.info("i = {}, codePoint = {}", i, str.get(i));
        }
        */
        
        
        return SString.fromList(tk.str);
    }

    // #( ... )
    @Override
    public Object toArray(boolean eofError, Object eofval) {

        // 先頭の(は、読んでいない（ちょっとここは特殊）
        Object list = read(eofError, eofval);

        int len = listLength(list);


        Object[] arr = new Object[len];

        int idx = 0;
        while (list instanceof Cell) {
            Cell cell = (Cell) list;

            arr[idx] = cell.getCar();
            idx++;
            list = cell.getCdr();
        }

        return new SArray(arr);
    }

    // #u8( ... )
    @Override
    public Object toU8Array(boolean eofError, Object eofval) {

        // 先頭の(は、読んでいない（ちょっとここは特殊）
        Object list = read(eofError, eofval);

        int len = listLength(list);

        byte[] arr = new byte[len];

        int idx = 0;
        while (list instanceof Cell) {
            Cell cell = (Cell) list;

            Number num = (Number) cell.getCar();
            arr[idx] = num.byteValue();
            idx++;
            list = cell.getCdr();
        }

        return new U8Array(arr);
    }
    
    public static int listLength(Object aList) {

        Object list = aList;

        for (int count = 0;; count++) {
            if (!(list instanceof Cell)) {
                return count;

            }
            Cell cell = (Cell) list;
            list = cell.getCdr();
        }
    }


}
