package uika806.syntax;

import java.util.List;
import org.slf4j.LoggerFactory;
import uika806.err.BadArgumentInFunctionException;
import uika806.kernel.AFn;
import uika806.port.CurrentPort;

/**
 *
 * @author hemmi
 */
public class DebugRulesFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(DebugRulesFn.class);
    

    @Override
    public String getName() {
        return "DebugRules";
    }

    @Override
    public Object invoke(Object arg1) {
        
        if (arg1 instanceof SyntaxRules) {
            SyntaxRules sr = (SyntaxRules) arg1;
            
            List<SyntaxRules.SyntaxRule> rules = sr.rules;
            int n = rules.size();
            for (int i = 0; i < n; i++) {
                SyntaxRules.SyntaxRule rule = rules.get(i);
                
                Object pat = rule.pattern;
                Object tmp = rule.template;
                
                LOG.info("rule[ {} ] = {}" , i, CurrentPort.printLong(pat) );
                LOG.info("             {}" , CurrentPort.printLong(tmp) );
                
                
            }
            
            
            
        } else {
            throw new BadArgumentInFunctionException("-rules");
        }
        
        return Boolean.FALSE;
    }

    
}
