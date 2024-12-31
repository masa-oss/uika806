/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import uika806.err.ReaderException;
import java.io.UnsupportedEncodingException;

/**
 * 
 * サロゲート・ペアではなく、１文字単位に Codepointで、処理する Stream
 *
 */
public final class CodepointLispStream implements InputPort {



    //   UNICODEの場合は、２バイトで収まらない事があるので、int[]にしている
    private final int[] str;
    private final int length;

    private int idx;
    private int lineNum = 1;
    private int nthChar = 1;
    boolean open = true;

    /**
     * Creates a new instance of LispStream
     *
     * @param str
     * @param length
     */
    private CodepointLispStream(int[] str, int length) {
        this.str = str;
        this.length = length;
        this.idx = 0;
    }


    public static CodepointLispStream fromUtf8(String string) throws UnsupportedEncodingException {

        byte[] from = string.getBytes("UTF8");

        int len = from.length;

        int[] to = new int[len];
        int j = 0;

        for (int i = 0; i < len;) {

            int ch = from[i++] & 255;
            if (ch < 128) {
                to[j] = ch;
                j++;
            } else if ((ch & 0xe0) == 0xc0) {
                // 2 byte
                int code = ch & 0x1f;
                ch = from[i++] & 255;
                to[j] = (code << 6) + (ch & 0x3f);
                j++;
            } else if ((ch & 0xf0) == 0xe0) {
                // 3 byte 
                int code = ch & 0x0f;
                ch = from[i++] & 0x3f;
                code = (code << 6) | ch;
                ch = from[i++] & 0x3f;
                to[j] = (code << 6) | (ch);
                j++;
            } else if ((ch & 0xf8) == 0xf0) {
                // 4 byte 
                int code = ch & 0x07;
                ch = from[i++] & 0x3f;   // 2 byte me
                code = (code << 6) | ch;
                ch = from[i++] & 0x3f;   //  3 byte me
                code = (code << 6) | ch;
                ch = from[i++] & 0x3f;   //  4 byte me
                to[j] = (code << 6) | (ch);
                j++;
            } else {
                throw new UnsupportedEncodingException("ch = " + ch);
            }

        }

        return new CodepointLispStream(to, j);
    }

    @Override
    public int getLineNum() {
        return lineNum;
    }

    @Override
    public int getNthChar() {
        return nthChar;
    }

    // &optional input-stream (eof-error-p t) eof-value recursive-p
    @Override
    public int readCodepoint() {
        if (idx >= length) {
            return -1;
        } else {
            int c = str[idx];

            if (c == '\n') {
                lineNum++;
                nthChar = 1;
            } else {
                nthChar++;
            }

            idx++;

            return c;
        }
    }

    // &optional peek-type input-stream (eof-error-p t) eof-value recursive-p
    /*
     * (a) If peek-type is nil, peek-char reads the next character
     *     from the current input stream and returns it as an object,
     *     but does not remove the character from the stream.
     *     This has the same effect as using read-char followed by
     *     unread-char.
     *
     * (b) If peek-type is t, peek-char reads the next character
     *     that is not a whitespace character from the current
     *     input stream and returns it as an object, but does not
     *     remove the character form the stream.
     *     NB: characters in comments are not ignored by peek-char.
     *
     * (c) If peek-type is a character object, peek-char skips over successive characters until a character that is char= to char is found. This character is returned by peek-char but is not removed from the stream.
     */
    @Override
    public int peekCodepoint() {
        if (idx >= length) {
            return -1;
        } else {
            return str[idx];
        }
    }

    @Override
    public String toString() {
        return "CodepointLispStream[lineNum = " + lineNum + ", nthChar = " + nthChar + "]";
    }

    @Override
    public void close() {
        open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
    
    @Override
    public boolean ready() {
        return true;
    }

    @Override
    public boolean isTextualPort() {
        return true;
    }
    
    @Override
    public boolean isBinaryPort() {
        return false;
    }

    @Override
    public int readByte() {
        throw new ReaderException("This port not support : readByte");
    }
    
    @Override
    public int peekU8() {
        throw new ReaderException("This port not support : peekU8");
    }
}
