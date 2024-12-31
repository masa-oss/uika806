package uika806.port;

import uika806.objects.U8Array;

/**
 *
 * @author hemmi
 */
public class ByteVectorInputPort implements InputPort {


    int idx = 0;
    U8Array arr;
    final int length;

    public ByteVectorInputPort(U8Array u8arr) {
        if (u8arr == null) throw new NullPointerException();
        this.arr = u8arr;
        this.length = u8arr.length();
    }

    @Override
    public int peekCodepoint() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public int readCodepoint() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public int getLineNum() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public int getNthChar() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void close() {
        arr = null;
    }

    @Override
    public boolean isOpen() {
        return arr != null;
    }
    
    
    @Override
    public boolean ready() {
        return true;
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
    public int readByte() {

        if (idx < length) {
            int by = arr.getNthAsInt(idx);
            idx++;
            return by;
        }
        return -1;
    }

    @Override
    public int peekU8() {
        
        if (idx < length) {
            int by = arr.getNthAsInt(idx);
            return by;
        }
        return -1;
    }

}
