/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.print;

import uika806.kernel.IPrinterFactory;
import uika806.kernel.IPrintable;

/**
 *
 * @author hemmi
 */
public class PrinterFactoryImpl implements IPrinterFactory {

    @Override
    public IPrintable newInstance() {


        return new PrinterSchemeEx();
    }
    
}
