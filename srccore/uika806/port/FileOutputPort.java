/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 *
 * Move package uika806.small.objects to xxx.port
 */
public class FileOutputPort implements OutputPort {

    @Override
    public boolean isOpen() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public void write(String str) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }


    String fileName;
    OutputStream os;
    Writer wr;

    public FileOutputPort(String fileName, OutputStream os, Writer wr) {
        this.fileName = fileName;
        this.os = os;
        this.wr = wr;

    }

    @Override
    public void write(int codePoint) throws IOException {
        
        StringBuilder sb = new StringBuilder();
        sb.appendCodePoint(codePoint);
        wr.write(sb.toString());
    }

    @Override
    public void close() {
        try {
            wr.flush();

        } catch (IOException ioe) {

        }

        try {
            os.close();

        } catch (IOException ioe) {

        }

    }

    @Override
    public void flush() {
        try {
            wr.flush();

        } catch (IOException ioe) {

        }
    }

    @Override
    public boolean isTextualPort() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public boolean isBinaryPort() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public void writeByte(int by) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }


}
