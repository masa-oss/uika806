/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import uika806.err.BadArgumentInFunctionException;
import uika806.err.FileException;
import uika806.kernel.AFn;
import uika806.port.ReaderInputStream;
import uika806.objects.SString;

/**
 *
 */
public class OpenInputFileFn extends AFn {

    @Override
    public String getName() {
        return "open-input-file";
    }

    @Override
    public Object invoke(Object arg1) {

        if (!(arg1 instanceof SString)) {
            throw new BadArgumentInFunctionException("open-input-file");
        }
        
        SString str = (SString) arg1;
        ReaderInputStream iPort = null;
        try {
            String fileName = str.toString();
            
            FileInputStream fos = new FileInputStream(fileName);
            BufferedInputStream bos = new BufferedInputStream(fos);
            InputStreamReader isw = new InputStreamReader(bos, "UTF-8");
            
            
            iPort = new ReaderInputStream(isw);
            
        } catch (Exception ex) {
            throw new FileException("open-input-file : " +  str.toString()  , ex);
        }
        return iPort;
    }

}
