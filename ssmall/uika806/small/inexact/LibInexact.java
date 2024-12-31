package uika806.small.inexact;

import uika806.kernel.AFn;
import uika806.objects.Complex;

/**
 *
 * @author hemmi
 */
public class LibInexact {

    public static class InfiniteqFn extends AFn {

        @Override
        public String getName() {
            return "infinite?";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Double) {
                Double db = (Double) arg1;

                return Double.isInfinite(db);
            }
            if (arg1 instanceof Complex) {
                Complex comp = (Complex) arg1;

                return Double.isInfinite(comp.re()) || Double.isInfinite(comp.im());
            }
            return Boolean.FALSE;
        }
    }

    public static class AcosFn extends AFn {

        @Override
        public String getName() {
            return "acos";
        }

        @Override
        public Object invoke(Object arg1) {

            double dbl = ((Number) arg1).doubleValue();

            return Math.acos(dbl);

        }
    }

    public static class AsinFn extends AFn {

        @Override
        public String getName() {
            return "asin";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {
                Number num = (Number) arg1;
                return Math.asin(num.doubleValue());
            }
            throw new IllegalArgumentException("asin");
        }
    }

    public static class AtanFn extends AFn {

        @Override
        public String getName() {
            return "atan";
        }

        @Override
        public Object invoke(Object arg1) {

            if ((arg1 instanceof Complex)) {
                throw new IllegalArgumentException("atan");
            }
            if ((arg1 instanceof Number)) {

                double d1 = ((Number) arg1).doubleValue();
                return Math.atan(d1);
            }
            throw new IllegalArgumentException("atan");
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if ((arg1 instanceof Complex) || (arg2 instanceof Complex)) {
                throw new IllegalArgumentException("atan");
            }
            if ((arg1 instanceof Number) && (arg2 instanceof Number)) {

                double d1 = ((Number) arg1).doubleValue();
                double d2 = ((Number) arg2).doubleValue();

                return Math.atan2(d1, d2);
            }
            throw new IllegalArgumentException("atan");
        }

    }

    public static class CosFn extends AFn {

        @Override
        public String getName() {
            return "cos";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {
                Number num = (Number) arg1;
                return Math.cos(num.doubleValue());
            }
            throw new IllegalArgumentException("cos");
        }

    }

    public static class SinFn extends AFn {

        @Override
        public String getName() {
            return "sin";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {
                Number num = (Number) arg1;
                return Math.sin(num.doubleValue());
            }
            throw new IllegalArgumentException("sin");
        }
    }

    public static class TanFn extends AFn {

        @Override
        public String getName() {
            return "tan";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {
                Number num = (Number) arg1;
                return Math.tan(num.doubleValue());
            }
            throw new IllegalArgumentException("tan");
        }

    }

    public static class LogFn extends AFn {

        @Override
        public String getName() {
            return "log";
        }

        @Override
        public Object invoke(Object arg1) {

            if (arg1 instanceof Number) {
                double d = ((Number) arg1).doubleValue();
                return Math.log(d);
            }
            throw new IllegalArgumentException("log");
        }

        @Override
        public Object invoke(Object arg1, Object arg2) {

            if (arg2 instanceof Long) {
                if (10 != ((Long) arg2).longValue()) {

                    double d = ((Number) arg1).doubleValue();
                    double d2 = ((Number) arg2).doubleValue();

                    return Math.log(d) / Math.log(d2);

                }

                if (arg1 instanceof Number) {
                    double d = ((Number) arg1).doubleValue();
                    return Math.log10(d);
                }

            }
            throw new IllegalArgumentException("log");

        }
    }



public static class FiniteqFn extends AFn {

    @Override
    public String getName() {
        return "finite?";
    }

    @Override
    public Object invoke(Object arg1) {
        
        if (arg1 instanceof Double) {
            Double db = (Double) arg1;
            
            return (Double.isFinite(db));
        } else if (arg1 instanceof Long) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }
    
}




    
    
    // disable construct
    private LibInexact() {
        
    }
}
