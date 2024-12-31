package uika806.small.inexact;

import uika806.err.LispException;
import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class Complexs {

    public static class RealPartFn extends AFn {

        @Override
        public String getName() {
            return "real-part";
        }

        @Override
        public Object invoke(Object arg1) {
            
            if (arg1 instanceof Complex) {
                Complex co = (Complex) arg1;
                return co.re();
            }
            throw new LispException("real-part");
        }
    }

    public static class ImagPartFn extends AFn {

        @Override
        public String getName() {
            return "imag-part";
        }

        @Override
        public Object invoke(Object arg1) {
            
            if (arg1 instanceof Complex) {
                Complex co = (Complex) arg1;
                return co.im();
            }
            throw new LispException("imag-part");
        }
    }
}
