/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import org.slf4j.LoggerFactory;
import uika806.err.ReaderException;
import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class StringInputPort implements InputPort {
    
    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(StringInputPort.class);
    
    int lineNo = 1;
    int charNo = 1;
    int idx = 0;
    SString str;
    final int length;
    
    public StringInputPort(SString str) {
        
        if (str == null) throw new NullPointerException();
        this.str = str;
        this.length = str.length();
        
        
        // test17.scmのバグ調査
        /*
        int len = str.length();

        for (int i = 0; i < len; i++) {
            LOG.info("i = {}, codePoint = {}", i, str.getNth(i));
        }
        */
    }

    @Override
    public int peekCodepoint() {
        
        if (idx >= length) {
            return -1;
        }
        
        int codepoint = str.getNth(idx);
        return codepoint;
    }

    @Override
    public int peekU8() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public int readCodepoint() {

        if (idx >= length) {
            return -1;
        }
        
        if (str == null) {
            throw new ReaderException("closed");
        }
        
        int codepoint = str.getNth(idx);
        if (codepoint == '\n') {
            this.lineNo++;
            charNo = 0;
        }
        charNo++;
        idx++;
        return codepoint;
    }

    @Override
    public int getLineNum() {
        return this.lineNo;
    }

    @Override
    public int getNthChar() {
        return this.charNo;
    }

    @Override
    public boolean ready() {
        return true;
    }

    @Override
    public int readByte() {
        throw new UnsupportedOperationException("Not supported yet.");
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
    public void close() {
        this.str = null;
    }
    
    @Override
    public boolean isOpen() {
        return this.str != null;
    }


}
