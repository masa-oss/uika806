package uika806.kernel;

import uika806.objects.SSymbol;

/**
 *
 * @author hemmi
 */
public interface ICompiler {

    void putMacroIfAbsent(SSymbol key, AFn value);
    
}
