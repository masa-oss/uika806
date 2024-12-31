/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.objects.Cell;

/**
 *
 */
public class EqualFn extends AFn {

    Eqvq2Fn eqvFn = new Eqvq2Fn();
    
    @Override
    public String getName() {
        return "equal";
    }
    

    @Override
    public Object invoke(Object arg1, Object arg2) {

        Equal2Helper help = new Equal2Helper();
        help.checkCircular(arg1);
        
    //    help.dumpResult();

        return equal(arg1, arg2, help);
    }


    boolean equal(Object arg1, Object arg2, Equal2Helper help) {
        
        
        Equal2Helper.ObjInfo found = help.findObject(arg1);
        if (found != null && found.refCount > 1) {
            
            return (arg1 == arg2);  // 巡回リストの無限ループ解消のため
        }

        if (arg1 instanceof Cell) {
            if (arg2 instanceof Cell) {
                Cell c1 = (Cell) arg1;
                Cell c2 = (Cell) arg2;
                
                boolean bCar = equal(c1.getCar(), c2.getCar(), help);
                if (bCar == false) return false;
                boolean bCdr = equal(c1.getCdr(), c2.getCdr(), help);
                return bCdr;
                
            } else {
                return false;
            }
        } else {
            if (arg2 instanceof Cell) {
                return false;
            } else {
              //  return arg1.equals(arg2);
                
                return (Boolean)eqvFn.invoke(arg1, arg2);
            }
        }
    }



}
