/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */

package uika806.fn011.reader;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class Token {

    public static int ID = 1;

    public int id;
    public List<Integer> str;
    public int line;

    /*  
    *  -1  Eof
    *
    *  0xf0 (240)     旧 -3  使っちゃダメな文字 {} [] | 漢字
    *  0x1e (30)     旧 -2  文字列read中のEOF-error
    *
    *  10  LF
    *  N  10進数   複素数含む
    *  
    *  (
    *  )
    *  ;   comment
    *  "   string
    *  #               (bool 含む　) (character含む)  #(含む #u8(含む
    *
    *  +    数字含む
    *  -    数字含む
    *  .   dot
    *
    *  '   quote          OK
    *  `   back_quote     OK
    *
    *  |   今のところエラー   #| しか出ないのでは？
    *
    *  a0  identity
    *  a1   ,             OK
    *  a2   ,@            OK
    *  a3   #| ... |#
    *  a4   #(             ただし ( は、戻す。 #だけ読んだ状態。
    *  a5   #u8(           ただし ( は、戻す。 #u8を読んだ状態。
    *  a6   #;
    */
    static List<Integer> EMPTY = Collections.unmodifiableList(new ArrayList<>());

    public Token(int id, List<Integer> str) {
        /*
        if (id == 240) {
            new IllegalArgumentException("240").printStackTrace();
        }
        */
        this.id = id;
        this.str = (str == null) ? new ArrayList<>() : str;
        this.line = -1;
    }

    public Token(int id, List<Integer> str, int line) {
        /*
        if (id == 240) {
            new IllegalArgumentException("240a").printStackTrace();
        }
        */

        this.id = id;
        this.str = (str == null) ? EMPTY : str;
        this.line = line;
    }

    public int getId() {
        return id;
    }

    public String getText() {

        StringBuilder sb = new StringBuilder();
        int len = str.size();
        for (int i = 0; i < len; i++) {
            int code = str.get(i);
            sb.appendCodePoint(code);
        }
        return sb.toString();
    }

    /**
     * 以下が返す文字列のフォーマットは、将来変更になる可能性があります。
     *
     * @return
     */
    @Override
    public String toString() {

        if (id == 59) {  // ;
            return "Token[ " + ";" + "  " + str.size() + "]";

        } else if (id == 10) {
            return "Token[ " + "LF" + "]";

        }

        StringBuilder sb = new StringBuilder();
        str.stream().forEach((i) -> sb.appendCodePoint(i));

        String decode = (' ' < id && id < 127) ? String.valueOf((char) id)
                : (id < 0) ? String.format("%d", id) :  "0x" +  Integer.toHexString(id);

        if (line > 0) {
            sb.append(", line=").append(line);
        }
        sb.append(']');

        return "Token[ " + decode + "  " + sb.toString();
    }

}
