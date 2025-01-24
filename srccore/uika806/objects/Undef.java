/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;

import java.util.Objects;
import uika806.kernel.PrintOption;
import uika806.kernel.SelfPrintable;
import uika806.port.OutputPort;

/**
 * Undefined object.
 */
public class Undef implements SelfPrintable {

    public static final Undef Undefined = new Undef();

    
    private Undef() {
    }

    @Override
    public void prin1(OutputPort outport, PrintOption opt) {
        
        outport.write("#<Undef>");
    }

    @Override
    public String toString() {
        return "Undef[]";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

}
