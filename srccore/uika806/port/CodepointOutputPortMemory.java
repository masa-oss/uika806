/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import uika806.objects.SString;
import java.util.ArrayList;
import uika806.err.ReaderException;


/**
 * このクラスは、PrinterSchemeExで使われている。
 * 将来的に、廃止する予定のため、使わない事を推奨。
 */
public class CodepointOutputPortMemory implements OutputPort {

    ArrayList<Integer> list = new ArrayList<>();
    boolean open = true;

    @Override
    public void write(int codePoint) {

        list.add(codePoint);
    }

    public SString toSString() {

        SString s = SString.fromList(list);
        return s;
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
    public void write(String str) {
        throw new ReaderException("This port not support : write");
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
        throw new ReaderException("This port not support : writeByte");
    }
}
