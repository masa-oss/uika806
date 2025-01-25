package uika806.pico.fn;

import uika806.err.LispException;
import uika806.kernel.AFn;
import uika806.objects.Cell;

/**
 * clojureのnth
 * 
 */
public class NthFn extends AFn {

    @Override
    public String getName() {
        return "nth";
    }

    static final Object NOT_FOUND = new Object();
    /**
     * (nth coll index)
     * @param arg1
     * @param arg2
     * @return 
     */
    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        Long ln = (Long)arg2;
        
        Object found = nth(arg1, ln.intValue(), NOT_FOUND);
        if (found == NOT_FOUND) {
            throw new LispException("not found");
        }
        return found;
    }

    /**
     * (nth coll index not-found)
     * @param arg1
     * @param arg2
     * @param arg3
     * @return 
     */
    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {

        Long ln = (Long)arg2;

        return nth(arg1, ln.intValue(), arg3);
    }


    Object nth(final Object coll, final int nth, final Object notfound) {
        
        Object list = coll;
        int nn = nth;
        for (;;) {
            if (nn < 0) {
                return notfound;
            }
            if (nn == 0) {
                if (list instanceof Cell) {
                    Cell cell = (Cell) list;
                    return cell.getCar();
                } else {
                    return notfound;
                }
            }
            
            if (list instanceof Cell) {
                Cell cel5 = (Cell) list;
                list = cel5.getCdr();
            } else {
                return notfound;
            }
            nn--;
        }
    }


    
}
