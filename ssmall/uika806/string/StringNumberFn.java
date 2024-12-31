package uika806.string;

import uika806.fn011.reader.NumParser;
import uika806.kernel.AFn;
import uika806.objects.SString;

/**
 *
 * @author hemmi
 */
public class StringNumberFn extends AFn {

    @Override
    public String getName() {
        return "string->number";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof SString) {
            SString s = (SString) arg1;
            NumParser parser = new NumParser(s.toString());
            
            if (parser.isFloat()) {
                
                return Double.parseDouble(s.toString());
            } else if (parser.isInt()) {
                
                return Long.parseLong(s.toString());
            }
            
            /*
            
            注: string-> number の定義域を実装は次のように制限し
            てもよい。もしも実装がサポートする数がすべて実数ならば，
            string->number は，string が複素数に対する極座標系または直 交座系表記を使った場合に対して常に #f を返すことが許可されて いる。
            
            */
        
            return Boolean.FALSE;
        }
        return Boolean.FALSE;
    }

    
    @Override
    public Object invoke(Object arg1, Object arg2) {

        if (arg1 instanceof SString) {
            SString s = (SString) arg1;
            if (arg2 instanceof Long) {
                Long radix = (Long) arg2;
                return Long.parseLong(s.toString(), radix.intValue());
            }
        }
        return Boolean.FALSE;
    }
        
    
}
