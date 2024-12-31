/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import uika806.err.ReaderException;

/**
 *
 * @author hemmi
 */
public class CodepointOutputPortImpl implements OutputPort {

    StringBuilder sb = new StringBuilder();
    boolean open = true;
    
    @Override
    public void write(String str) {
        sb.append(str);
    }

    @Override
    public void write(int codePoint) {
        sb.appendCodePoint(codePoint);
    }

    @Override
    public void flush() {
    }

    @Override
    public void close() {
        open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
    
/*  2024-11-08  
    public String toString() {
        return sb.toString();
    }
*/
    
    public String getAsString() {
        return sb.toString();
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
    public void writeByte(int by) {
        throw new ReaderException("This port not support : writeByte");
    }
}
