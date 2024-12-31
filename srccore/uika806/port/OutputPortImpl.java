/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;
import java.io.OutputStreamWriter;
import org.slf4j.LoggerFactory;
import uika806.err.FileException;

/**
 *
 * @author hemmi
 */
public class OutputPortImpl implements OutputPort {

    @Override
    public boolean isOpen() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }
    
    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(OutputPortImpl.class);
    
    
    OutputStreamWriter osw;
    
    public OutputPortImpl(OutputStreamWriter osw) {
        this.osw = osw;
    }

    @Override
    public void write(int codePoint) throws IOException {
        
        StringBuilder sb = new StringBuilder();
        sb.appendCodePoint(codePoint);
        write(sb.toString());
    }

    @Override
    public void flush() {
        try {
            osw.flush();
        } catch (IOException ex) {
            throw new FileException("IOException", ex);
        }
    }

    @Override
    public void writeByte(int by) {
        throw new UnsupportedOperationException("Not supported yet.");
        // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public void write(String str) {
        try {
            osw.write(str);
        } catch (IOException ex) {
            throw new FileException("IOException", ex);
        }
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
        
        
        try {
            osw.close();
        } catch (IOException ex) {
            throw new FileException("IOException", ex);
        }
    }
    
}
