/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

/**
 * このクラスは、PrinterSchemeExで使われている。
 * 将来的に、廃止する予定のため、使わない事を推奨。
 */
public class CodepointOutputPortNop implements OutputPort {

    @Override
    public boolean isOpen() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void write(String str) {
        throw new UnsupportedOperationException("Not supported yet.");
    }


    @Override
    public void write(int codePoint) {
    }

    @Override
    public void close() {
    }

    @Override
    public void flush() {
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
        throw new UnsupportedOperationException("Not supported yet.");
    }
    
    
}
