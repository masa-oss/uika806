/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;
import java.util.ArrayList;
import org.slf4j.LoggerFactory;
import uika806.err.FileException;

/**
 *
 * @author hemmi
 */
public class ByteVectorOutputPort implements OutputPort {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(ByteVectorOutputPort.class);
    

    public ArrayList<Byte> list = new ArrayList<>();
    boolean open = true;

    public ByteVectorOutputPort() {
    }

    @Override
    public void write(int codePoint) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void flush() {
    }

    @Override
    public void writeByte(int by) {

        if (!open) {
            throw new FileException("It was already closed.");
        }

        if (0 <= by && by < 256) {

            byte by0 = (byte) by;
            list.add(by0);

        } else {
            throw new IllegalArgumentException("argument must be 0 to 255");
        }
    }

    @Override
    public void write(String str) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean isTextualPort() {
        return false;
    }

    @Override
    public boolean isBinaryPort() {
        return true;
    }

    @Override
    public void close() {
        open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

}
