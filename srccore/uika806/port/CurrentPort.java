/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.port;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Reader;

import uika806.print.PrinterSchemeLimit;

/**
 *
 *
 */
public final class CurrentPort {

    public static InputPort INPUT_PORT;

    public static OutputPort OUTPUT_PORT;

    public static OutputPort ERROR_PORT;

    @Deprecated
    public static OutputStreamWriter OUTPUT_WRITER;

    static PrinterSchemeLimit pr = new PrinterSchemeLimit();

    public static String printString(Object obj) {
        return pr.prin1(obj);
    }

    static PrinterSchemeLimit prLong = new PrinterSchemeLimit(100, 100);
    
    public static String printLong(Object obj) {
        return prLong.prin1(obj);
    }
    
    
    public static void init(boolean reverse) throws IOException {

        PrintStream out = System.err;
        OutputStreamWriter w = new OutputStreamWriter(out, "UTF-8");

        PrintStream err = System.out;
        OutputStreamWriter w2 = new OutputStreamWriter(err, "UTF-8");

        if (reverse) {
            CurrentPort.OUTPUT_PORT = new OutputPortImpl(w);
            CurrentPort.ERROR_PORT = new OutputPortImpl(w2);

        } else {
            CurrentPort.OUTPUT_PORT = new OutputPortImpl(w2);
            CurrentPort.ERROR_PORT = new OutputPortImpl(w);

        }

        InputStream is = System.in;
        Reader r = new InputStreamReader(is, "UTF-8");
        CurrentPort.INPUT_PORT = new ReaderInputStream(r);
    }

    // disable construct
    private CurrentPort() {
        
    }
}
