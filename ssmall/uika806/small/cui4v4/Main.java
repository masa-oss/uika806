/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.cui4v4;

import java.io.IOException;
import org.slf4j.LoggerFactory;

import uika806.Services;
import uika806.small.env.EnvironFactory;
import uika806.small.env.BuiltInFuncsImpl2;
import uika806.fn011.reader.Tokenizer;
import uika806.kernel.VMLogger;

import uika806.port.CodepointOutputPortImpl;
import uika806.port.InputPort;
import uika806.port.OutputPort;
import uika806.print.PrinterShared;
import uika806.port.CurrentPort;

import uika806.reader.LispReaderEx;
import uika806.reader.LispReaderFx;
import uika806.syntax.Environ;
import uika806.vm4.Compile4;
import uika806.vm4.Op;
import uika806.vm4.VM4;

/**
 *
 * @author hemmi
 */
public class Main {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(Main.class);

    public static void main(String args[]) {

        try {
            //     escapeTest();
            String property = System.getProperty("java.version");
            String property2 = System.getProperty("java.vendor");
            System.out.println("java.version=" + property);
            System.out.println("java.vendor=" + property2);
        } catch (RuntimeException ioe) {
            ioe.printStackTrace();
            return;
        }

        try {
            CurrentPort.init(false);

            EnvironFactory factory = new EnvironFactory();
            factory.loadBase();

            Services.environFactory = factory;

            Environ lexEnv = factory.getFirstEnviron(true, true);

            InputPort zis = CurrentPort.INPUT_PORT;
            OutputPort osw = CurrentPort.OUTPUT_PORT;

            for (;;) {
                Object o = null;
                try {
                    o = repl(zis, osw, lexEnv);
                } catch (Exception ex) {

                    String msg = "Error(" + ex.getMessage() + ")";
                    osw.write(msg);
                    osw.flush();
                }
                if (o == EOF) {
                    break;
                }
            }
            System.out.println("----------- END");

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    static Object EOF = new Object();

    static Object repl(InputPort zis, OutputPort osw, Environ lexEnv) throws IOException {

        osw.write("\n> ");
        osw.flush();

        // Read
        Object sexp = null;
        Tokenizer tk = new Tokenizer(zis);
        LispReaderEx reader = new LispReaderFx(tk);
        sexp = reader.read(false, EOF);
        if (sexp == EOF) {
            return EOF;
        }

        // Compile
        //   VMLogger logger = new VM4Logger();
        VMLogger logger = null;
        Compile4 comp = new Compile4(logger, lexEnv);
        Op op = comp.invoke(sexp, Op.HALT);

        // VM
        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();
        VM4 vm = new VM4(op, lexEnv, logger, bu);

        sexp = vm.exec();

        // Print
        CodepointOutputPortImpl outPort = new CodepointOutputPortImpl();
        new PrinterShared().prin1(sexp, outPort);

      //  osw.write(outPort.toString());
        osw.write(outPort.getAsString());
        osw.flush();
        return sexp;
    }

}
