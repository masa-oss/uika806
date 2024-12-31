package uika806.small.fn;

import uika806.err.BadArgumentInFunctionException;
import uika806.print.PrinterShared;
import uika806.kernel.AFn;
import uika806.port.OutputPort;
import uika806.kernel.PrintOption;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;

/**
 *
 * @author hemmi
 */
public class DisplaySmallFn extends AFn {

    PrinterShared logic = new PrinterShared(PrintOption.DISPLAY);

    @Override
    public String getName() {
        return "display";
    }

    @Override
    public Object invoke(Object arg1) {

        Object out = CurrentPort.OUTPUT_PORT;
        return invoke(arg1, out);
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {

        if (arg2 instanceof OutputPort) {

            OutputPort cop = (OutputPort) arg2;
            logic.prin1(arg1, cop);
            cop.flush();

        } else {
            throw new BadArgumentInFunctionException("Bad argument for display "+ arg1);
        }
        return SSymbol.Undefined;
    }
}
