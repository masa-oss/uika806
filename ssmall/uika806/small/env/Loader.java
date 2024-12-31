/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.env;

import java.io.IOException;
import org.slf4j.LoggerFactory;

import uika806.port.InputPort;
import uika806.reader.LispReaderEx;
import uika806.fn011.reader.Tokenizer;
import uika806.kernel.VMLogger;
import uika806.objects.SString;

import uika806.pico.fn.CloseInputPortFn;
import uika806.pico.fn.OpenInputFileFn;

import uika806.reader.LispReaderFx;
import uika806.port.CurrentPort;
import uika806.vm4.Compile4;
import uika806.vm4.Op;
import uika806.vm4.VM4;
import uika806.syntax.Environ;

/**
 *
 * Move haruka.v4 to uika806.cui4
 */
public class Loader {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(Loader.class);

    OpenInputFileFn openFn = new OpenInputFileFn();
    CloseInputPortFn closeFn = new CloseInputPortFn();

    public void load(String iniFile, Environ lexEnv) throws IOException {

        Object eof = new Object();
        SString s = SString.fromString(iniFile);
        Object port = openFn.invoke(s);
        try {

            LOG.info("+++++++++++++++++++  {}", port);

            for (;;) {
                Object sexp = null;
                Tokenizer tk = new Tokenizer((InputPort) port);
                LispReaderEx reader = new LispReaderFx(tk);
                sexp = reader.read(false, eof);

                if (sexp == eof) {
                    break;
                }
                LOG.info("38) ----- {}", CurrentPort.printString( sexp));

                eval(sexp, lexEnv);
            }

        } finally {
            closeFn.invoke(port);
        }
    }

    void eval(Object sexp, Environ lexEnv) {

        VMLogger log = null;
        Compile4 comp = new Compile4(log, lexEnv);

        Op op = comp.invoke(sexp, Op.HALT);

        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();

        VM4 vm = new VM4(op, lexEnv, log, bu);

        Object obj = vm.exec();

    }

}
