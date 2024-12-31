/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import java.math.BigInteger;
import uika806.objects.Complex;
import uika806.objects.Ratio;

/**
 *
 * @author hemmi
 */
public final class Numbers {

    public static Object add(Object arg1, Object arg2) {

        if (arg1 instanceof Complex) {
            return addComp((Complex) arg1, (Complex) arg2);

        } else if (arg1 instanceof Ratio) {
            return addRatio((Ratio) arg1, toRatio (arg2));

        } else if (arg2 instanceof Ratio) {
            return addRatio( toRatio (arg1), (Ratio) arg2);
            
        } else {
            Number i1 = (Number) arg1;
            long wk = i1.longValue();

            long i2 = ((Number) arg2).longValue();
            wk = Math.addExact(wk, i2);
            return wk;
        }
    }
    
    static Ratio toRatio(Object o) {
        
        if (o == null) throw new NullPointerException();
        if (o instanceof Ratio) {
            return (Ratio)o;
        }
        if (o instanceof Long) {
            return new Ratio((Long)o, 1L);
        }
        throw new RuntimeException("not impl yet : "  + o.getClass().getName() );
        
    }
    
    

    public static Object addRatio(Ratio arg1, Ratio arg2) {

        BigInteger bunshi1 = arg1.getNumerator().multiply(arg2.getDenominator());

        BigInteger bunshi2 = arg2.getNumerator().multiply(arg1.getDenominator());

        BigInteger bunbo = arg1.getDenominator().multiply(arg2.getDenominator());

        return new Ratio(bunshi1.add(bunshi2), bunbo);
    }

    public static Object addComp(Complex arg1, Complex arg2) {

        return new Complex(                arg1.re() + arg2.re(), arg1.im() + arg2.im());

    }

    public static Object minus(Object arg1, Object arg2) {

        if (arg1 instanceof Complex) {
            return subComp((Complex) arg1, (Complex) arg2);

        } else if (arg1 instanceof Ratio) {
            return subRatio((Ratio) arg1, (Ratio) arg2);

        } else {

            Number i1 = (Number) arg1;
            long wk = i1.longValue();

            long i2 = ((Number) arg2).longValue();
            wk = Math.subtractExact(wk, i2);
            return wk;
        }
    }

    public static Object subComp(Complex arg1, Complex arg2) {

        return new Complex(
                arg1.re() - arg2.re(), arg1.im() - arg2.im());
    }

    public static Object subRatio(Ratio arg1, Ratio arg2) {

        BigInteger bunshi1 = arg1.getNumerator().multiply(arg2.getDenominator());

        BigInteger bunshi2 = arg2.getNumerator().multiply(arg1.getDenominator());

        BigInteger bunbo = arg1.getDenominator().multiply(arg2.getDenominator());

        return new Ratio(bunshi1.subtract(bunshi2), bunbo);

    }

    public static Object multiply(Object arg1, Object arg2) {

        if (arg1 instanceof Complex) {
            return mulComp((Complex) arg1, (Complex) arg2);

        } else if (arg1 instanceof Ratio) {
            return mulRatio((Ratio) arg1, (Ratio) arg2);

        } else {

            Number i1 = (Number) arg1;
            long wk = i1.longValue();

            long i2 = ((Number) arg2).longValue();
            wk = Math.multiplyExact(wk, i2);
            return wk;
        }
    }

    public static Object mulComp(Complex arg1, Complex arg2) {

        double real = arg1.re() * arg2.re() - arg1.im() * arg2.im();

        double im = arg1.re() * arg2.im() + arg1.im() * arg2.re();
        return new Complex(real, im);
    }

    public static Object mulRatio(Ratio arg1, Ratio arg2) {

        BigInteger bunshi1 = arg1.getNumerator().multiply(arg2.getNumerator());

        BigInteger bunbo = arg1.getDenominator().multiply(arg2.getDenominator());
        return new Ratio(bunshi1, bunbo);
    }

    public static Object divide(Object arg1, Object arg2) {

        if (arg1 instanceof Complex) {
            return divComplex((Complex) arg1, (Complex) arg2);

        } else if (arg1 instanceof Ratio) {
            
            if (arg2 instanceof Ratio) {
                return divRatio((Ratio) arg1, (Ratio) arg2);
            } else if (arg2 instanceof Long) {
                BigInteger b1 = BigInteger.valueOf((Long)arg2);
                BigInteger b2 = BigInteger.valueOf(1L);
                Ratio r2 = new Ratio(b1, b2);
                return divRatio((Ratio) arg1, r2);
                
            } else {
                throw new RuntimeException("not impl yet:" + arg2.getClass().getName());
                
            }
            
        } else if (arg2 instanceof Ratio) {
            
            if (arg1 instanceof Ratio) {
                return divRatio((Ratio) arg1, (Ratio) arg2);
            } else if (arg1 instanceof Long) {
                Ratio r1 = new Ratio( (Long) arg1,    1L);
                return divRatio(r1, (Ratio) arg2);
                
            } else {
                throw new RuntimeException("not impl yet:" + arg1.getClass().getName());
                
            }
            
            
        } else {

            Long lng1 = (Long) arg1;
            Long lng2 = (Long) arg2;
            
            if ((lng1 % lng2) != 0) {
                
                BigInteger b1 = BigInteger.valueOf(lng1);
                BigInteger b2 = BigInteger.valueOf(lng2);
                return new Ratio(b1, b2);
            } else {

                return lng1 / lng2;
            }
        }
    }
    
    public static Object divComplex(Complex arg1, Complex arg2) {
    
        throw new RuntimeException("not impl yet");
    
    }
    
    public static Object divRatio(Ratio arg1, Ratio arg2) {
    
        BigInteger bunshi1 = arg1.getNumerator().multiply(arg2.getDenominator());

        BigInteger bunbo = arg1.getDenominator().multiply(arg2.getNumerator());
        return new Ratio(bunshi1, bunbo);
    
    }

    public static Object lt(Object arg1, Object arg2) {

        Long lng1 = (Long) arg1;
        Long lng2 = (Long) arg2;

        return lng1 < lng2;
    }

    public static Object gt(Object arg1, Object arg2) {

        Long lng1 = (Long) arg1;
        Long lng2 = (Long) arg2;

        return lng1 > lng2;
    }

    public static Object equiv(Number arg1, Number arg2) {

        return arg1.equals(arg2);
    }

    
    
    
    
    
    // diasble construct
    private Numbers() {
    }
}
