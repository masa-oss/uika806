/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 */
public class StringOutputPort implements OutputPort {

    
    boolean bOpen = true;
    private final List<Integer> list = new ArrayList<>();
    
    public StringOutputPort() {
    }

    @Override
    public void write(int codePoint) throws IOException {
        
        // TODO 範囲チェック
        list.add(codePoint);
    }
    
    
    public List<Integer>  getUnmodifiableList() {
        
        return Collections.unmodifiableList(list);
    }
    
    
    public String getAsString() {
        
        
        StringBuilder sb = new StringBuilder();
        list.forEach((codePoint) -> sb.appendCodePoint( codePoint ));
        
        return sb.toString();
    }

    
    
    @Override
    public void flush() {
    }

    @Override
    public void writeByte(int by) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void write(String str) {
        
        str.codePoints().forEach((codePoint) -> list.add(codePoint));
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
        bOpen = false;
    }
    @Override
    public boolean isOpen() {
        return bOpen;
    }
    
}
