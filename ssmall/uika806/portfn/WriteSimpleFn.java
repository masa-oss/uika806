package uika806.portfn;

import uika806.err.BadArgumentInFunctionException;
import uika806.kernel.AFn;
import uika806.port.OutputPort;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;
import uika806.print.PrinterSchemeLimit;

/**
 *
 * @author hemmi
 */
public class WriteSimpleFn extends AFn {
    
    PrinterSchemeLimit logic = new PrinterSchemeLimit(1000, 1000);

//    PrinterSmall logic = new PrinterSmall(PrintOption.WRITE);

    @Override
    public String getName() {
        return "write-simple";
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
            String str = logic.prin1(arg1);
            cop.write(str);
            cop.flush();

        } else {
            throw new BadArgumentInFunctionException("Bad argument for display "+ arg1);
        }
        return SSymbol.Undefined;
    }
}
