package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 * @author hemmi
 */
public class ListqFn extends AFn {

    CircularListq circular = new CircularListq();

    LastFn lastFn = new LastFn();
    
    
    @Override
    public String getName() {
        return "list?";
    }


    @Override
    public Object invoke(Object arg1) {

        Boolean b = (Boolean) circular.invoke(arg1);
        if (b) {
            // 循環リスト なら、false
            return Boolean.FALSE;
        }
        if (arg1 instanceof EmptyList) {
            return Boolean.TRUE;

        }

        Object last = lastFn.invoke(arg1);
        if (last instanceof Cell) {
            Object cdr = ((Cell) last).getCdr();
            if (cdr instanceof EmptyList) {
                return Boolean.TRUE;
            }

        }
        return Boolean.FALSE;
    }

}
