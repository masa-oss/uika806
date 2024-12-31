package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;

/**
 *
 * @author hemmi
 */
public class LastFn extends AFn {

    @Override
    public String getName() {
        return "last";
    }

    @Override
    public Object invoke(Object o) {

        if (o instanceof Cell) {
            Cell cell = (Cell) o;

            for (;;) {
                Object cdr = cell.getCdr();
                if (!(cdr instanceof Cell)) {
                    return cell;
                }
                cell = (Cell) cdr;
            }

        }
        throw new IllegalArgumentException();

    }

}
