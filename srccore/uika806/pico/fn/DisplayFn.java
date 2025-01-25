package uika806.pico.fn;

import java.io.IOException;


import uika806.kernel.AFn;
import uika806.err.LispException;
import uika806.objects.SSymbol;

import uika806.print.PrinterSchemeEx;

import uika806.port.CurrentPort;
import uika806.objects.SString;
import uika806.objects.Undef;
import uika806.port.OutputPort;

/**
 * <code>
 * 
 * gosh$ (display "abc")
abc#<undef>
gosh$ (write "abc")
"abc"#<undef>
 * </code>
 * @author hemmi
 */
public class DisplayFn extends AFn  {

    PrinterSchemeEx logic = new PrinterSchemeEx();
    
    @Override
    public String getName() {
        return "display";
    }

    
    
    @Override
    public Object invoke(Object body) {

        
        if (CurrentPort.OUTPUT_WRITER == null) throw new NullPointerException();
        
        SString prin1 = logic.prin1(body);
        
        
        try {
            CurrentPort.OUTPUT_WRITER.append(prin1.toString());
          //  RT.OUTPUT_WRITER.append("\n");
            CurrentPort.OUTPUT_WRITER.flush();
            
        } catch (IOException ioe) {
            throw new RuntimeException("IOException", ioe);
        }
        return Undef.Undefined;
    }
    
    PrinterSchemeEx printLogic = new PrinterSchemeEx();
    
    @Override
    public Object invoke(Object arg1, Object arg2) {

        if (arg2 instanceof OutputPort) {
            
            OutputPort cop = (OutputPort) arg2;
            
            printLogic.prin1(1, cop);
            
        } else {
            throw LispException.illegalArgument("Bad argument for display", arg1);
        }
        
        return Undef.Undefined;

    }
    
}
