package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;

/**
 *
 * @author hemmi
 */
public class CircularListq extends AFn {

    @Override
    public String getName() {
        return "circular-list?";
    }

    @Override
    public Object invoke(Object xs) {

        Object fast = xs;
        Object slow = xs;
        for (;;) {
            fast = cddr(fast);
            slow = cdr(slow);

            if (!(fast instanceof Cell) ) {
                return Boolean.FALSE;
            }
            if (fast == slow) {
                return Boolean.TRUE;
            }
        }
    }
    
    Object cdr(Object x) {
        
        if (x instanceof Cell) {
            
            Object y = ((Cell )x).getCdr();
            return y;
        }
        return RT.EOL;
    }

    Object cddr(Object x) {
        
        if (x instanceof Cell) {
            
            Object y = ((Cell )x).getCdr();
            if (y instanceof Cell) {
                return ((Cell) y).getCdr();
            }
            return RT.EOL;
        }
        return RT.EOL;
    }

}
