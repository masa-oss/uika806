package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.objects.EmptyList;

/**
 *
 * Move uika806.small.fn to xxx.pico.fn
 */
public class ListFn extends AFn {

    @Override
    public String getName() {
        return "list";
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {

        return new Cell(arg1, new Cell(arg2, new Cell(arg3,
                new Cell(arg4, new Cell(arg5, new Cell(arg6, EmptyList.NIL))))));
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {

        return new Cell(arg1, new Cell(arg2, new Cell(arg3,
                new Cell(arg4, new Cell(arg5, EmptyList.NIL)))));
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {

        return new Cell(arg1, new Cell(arg2, new Cell(arg3, new Cell(arg4, EmptyList.NIL))));
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {

        return new Cell(arg1, new Cell(arg2, new Cell(arg3, EmptyList.NIL)));
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        return new Cell(arg1, new Cell(arg2, EmptyList.NIL));
    }

    @Override
    public Object invoke(Object arg1) {
        return new Cell(arg1, EmptyList.NIL);
    }

    @Override
    public Object invoke() {
        return EmptyList.NIL;
    }

}
