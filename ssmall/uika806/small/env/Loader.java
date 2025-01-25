/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.env;

import java.io.IOException;
import org.slf4j.LoggerFactory;
import uika806.err.RaiseException;


import uika806.fn011.reader.Tokenizer;
import uika806.kernel.RT;
import uika806.kernel.VMLogger;

import uika806.objects.SString;
import uika806.pico.fn.CloseInputPortFn;
import uika806.pico.fn.OpenInputFileFn;
import uika806.port.CurrentPort;
import uika806.port.InputPort;

import uika806.reader.LispReaderEx;
import uika806.reader.LispReaderFx;
import uika806.syntax.Environ;

import uika806.vm4.Op;
import uika806.vm4.VM4;
import uika806.vm4.Compile5;



public class Loader {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(Loader.class);

    OpenInputFileFn openFn = new OpenInputFileFn();
    CloseInputPortFn closeFn = new CloseInputPortFn();

    public void load(String iniFile, Environ lexEnv) throws IOException {

        Object eof = new Object();
        SString sFileName = SString.fromString(iniFile);
        Object port = openFn.invoke(sFileName);
        try {

            LOG.info("+++++++++++++++++++  {}", port);
            InputPort inPort = (InputPort) port;

            for (;;) {
                
                Tokenizer tk = new Tokenizer(inPort);
                LispReaderEx reader = new LispReaderFx(tk);
                Object sexp = reader.read(false, eof);

                if (sexp == eof) {
                    break;
                }
                String str = CurrentPort.printLong( sexp);
                LOG.info("=============== {}", str);

                try {
                    eval(sexp, lexEnv);
                } catch (RuntimeException re) {
                    
                    LOG.error("61) =====================  RuntimeException", re);
                    
                    int line = inPort.getLineNum();
                    int nthChar = inPort.getNthChar();
                    Object errInfo = errorToList(line, nthChar, sFileName);
                    
                    RaiseException rai = new RaiseException(RaiseException.EVAL_EXCEPTION, "startup file error", errInfo, false);
                    throw rai;
                }
            }
        } finally {
            closeFn.invoke(port);
        }
    }

    
    Object errorToList(int line, int nchar, SString fileName) {
     
        Integer iLine = line;
        Integer iNchar = nchar;
        
        return RT.list(fileName, iLine, iNchar);
    }
    
    
    
    
    void eval(Object sexp, Environ lexEnv) {

        VMLogger log = null;
        Compile5 comp = new Compile5(log);

        Op op = comp.invoke(sexp, Op.HALT, lexEnv);

        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();

        VM4 vm = new VM4(op, lexEnv, log, bu);

        Object ignore = vm.exec();
    }

}
