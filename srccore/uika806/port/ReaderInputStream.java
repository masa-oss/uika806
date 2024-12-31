/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;
import java.io.Reader;
import uika806.err.ReaderException;

public final class ReaderInputStream implements InputPort {


    final Reader pbr;
    /**
     * PushbackReaderは 0 - FFFFの範囲しか使えなかった。
     * 
     * unread処理用に、int（１文字）を用意した。
     */
    boolean unreadFlag = false;
    int unreadCP = 0;

    
    public ReaderInputStream(Reader pbr) {
        this.pbr = pbr;
    }


    int readCodePoint() throws IOException {
        
        if (unreadFlag) {
            unreadFlag = false;
            return unreadCP;
        } else {
            int ch = pbr.read();
            if (ch != -1 && Character.isSurrogate((char) ch)) {
                int second = pbr.read();
                char[] arr = new char[2];
                arr[0] = (char) ch;
                arr[1] = (char) second;

                String s = new String(arr);
                return s.codePointAt(0);
            }
            return ch;
        }
    }

    void unreadCodePoint(int codePoint) {

        unreadFlag = true;
        unreadCP = codePoint;
    }

    @Override
    public int peekCodepoint() {

        int ch = 0;
        try {

            ch = readCodePoint();
            unreadCodePoint(ch);

        } catch (IOException ioe) {
          //  throw new RuntimeException("IOException", ioe);
            throw new ReaderException("IOException", ioe, 0, 0);
        }
        return ch;
    }

    @Override
    public int readCodepoint() {
        
        int ch = 0;
        try {
            ch = readCodePoint();
        } catch (IOException ioe) {
         //   throw new RuntimeException("IOException", ioe);
            throw new ReaderException("IOException", ioe, 0, 0);
        }

        if (ch == '\n') {
            lineNum++;
            nthChar = 1;
        } else {
            nthChar++;
        }

        return ch;
    }

    private int lineNum = 1;
    private int nthChar = 1;

    @Override
    public int getLineNum() {
        return lineNum;
    }

    @Override
    public int getNthChar() {
        return nthChar;
    }
    
    @Override
    public boolean isOpen() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void close() {
        
        try {
            this.pbr.close();
        } catch (IOException ioe) {
           // throw new LispException("io-error", ioe);
            throw new ReaderException("IOException", ioe, 0, 0);
        }
    }

    @Override
    public boolean ready() {

        boolean result = false;
        try {
            result = this.pbr.ready();
        } catch (IOException ioe) {
          //  throw new LispException("io-error", ioe);
            throw new ReaderException("IOException", ioe, 0, 0);
        }
        return result;
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

        throw new ReaderException("This port not support : readByte"  );
    }

    @Override
    public int peekU8() {
        throw new UnsupportedOperationException("Not supported yet.");
    }


}
