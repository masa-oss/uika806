/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.kernel.VMLogger;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;

/**
 *
 * @author hemmi
 */
public class VM4Logger implements VMLogger  {

    private static final Logger LOG = LoggerFactory.getLogger(VM4.class);
    
    public VM4Logger() {
        
    }
    
    @Override
    public void writeLog(Object a, Object x, Environ e, Object r, Object s) {
        
        Op op = (Op) x;
        
        String strA = CurrentPort.printString(a);
        String strX = op.displaySimply();
        String strR = CurrentPort.printString(r);   // ここもループする
        String strS = "dump=" + CurrentPort.printString(s);
        String    strE = e.printEnv();
        LOG.info("----------------------");
        LOG.info("IN: accm : {}", strA);
        LOG.info("IN: expr : {}", strX);
        LOG.info("IN: env : {}", strE);
        LOG.info("IN: r, argument={}", strR);
        LOG.info("IN: s {}", strS);

    }
    
    @Override
    public String printString(Object obj) {
        return CurrentPort.printString(obj);
    }
}
