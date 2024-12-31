package uika806.small.fn;

import uika806.err.BadArgumentInFunctionException;
import uika806.reader.LispReaderEx;
import uika806.fn011.reader.Tokenizer;
import uika806.kernel.AFn;
import uika806.objects.EndOfFile;
import uika806.port.CurrentPort;
import uika806.reader.LispReaderFx;
import uika806.port.InputPort;

/**
 *
 * @author hemmi
 */
public class ReadFn extends AFn {

    @Override
    public String getName() {
        return "read";
    }

    public static Object EOF = EndOfFile.INSTANCE;
    
    
    @Override
    public Object invoke() {
        return invoke( CurrentPort.INPUT_PORT );
    }
    
    @Override
    public Object invoke(Object arg1) {
        
        if (arg1 instanceof InputPort) {
            InputPort cop = (InputPort) arg1;
            Tokenizer tkn = new Tokenizer(cop);
            
            LispReaderEx reader = new LispReaderFx(tkn);
            return reader.read(false, EOF);
        } else {
            
            String clazz = (arg1 == null)? "null" : arg1.getClass().getName();
                    
            throw new BadArgumentInFunctionException("Bad argument for read : " + clazz);
        }
    }
    
}
