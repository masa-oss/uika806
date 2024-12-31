package uika806.small.fn;

import uika806.kernel.AFn;

/**
 *
 * @author hemmi
 */
public class ExptFn extends AFn {

    @Override
    public String getName() {
        return "expt";
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {

        if (arg1 instanceof Long) {

            long ln = ((Long) arg1).longValue();
            if (arg2 instanceof Long) {
                int n = ((Number) arg2).intValue();

                long result = 1L;
                for (int i = 0; i < n; i++) {
                    result = Math.multiplyExact(ln, result);
                }
                return result;
            } else if (arg2 instanceof Double) {
                int n = ((Number) arg2).intValue();

                double result = 1L;
                for (int i = 0; i < n; i++) {
                    result = ln * result;
                }
                return result;
                
            } else {
                throw new IllegalArgumentException("expt");
            }
        } else if (arg1 instanceof Double) {

            double ln = ((Double) arg1).longValue();
            int n = ((Number) arg2).intValue();

            double result = 1L;
            for (int i = 0; i < n; i++) {
                result = ln * result;
            }
            return result;
        }
        throw new IllegalArgumentException("expt");
    }

}
