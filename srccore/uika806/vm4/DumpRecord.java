/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import uika806.syntax.Environ;

/**
 *
 * @author hemmi
 */
public final class DumpRecord {
    
    Op op;
    Environ env;
    Object rr;
    DumpRecord ss;
    
    DumpRecord(Op ope, Environ le, Object arg, DumpRecord next) {
        this.op = ope;
        this.env = le;
        this.rr = arg;
        this.ss = next;
    }
    
    @Override
    public String toString() {
        return "DumpRecord[" + op + "]";
        
    }
    
}
