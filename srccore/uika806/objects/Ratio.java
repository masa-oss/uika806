/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.objects;


import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;


import uika806.err.FileException;
import uika806.kernel.PrintOption;
import uika806.kernel.SelfPrintable;
import uika806.port.OutputPort;

/**
 *
 * @author hemmi
 */
public class Ratio extends Number implements SelfPrintable {

    private final BigInteger numerator;
    private final BigInteger denominator;


    public Ratio(BigInteger numerator, BigInteger denominator) {
        this.numerator = numerator;
        this.denominator = denominator;
    }

    
    public Ratio(Long numerator, Long denominator) {
        
        BigInteger nu = BigInteger.valueOf(numerator);
        BigInteger de = BigInteger.valueOf(denominator);
        
        this.numerator = nu;
        this.denominator = de;
    }
    
    
    public Ratio normalize() {

        BigInteger bunbo = (denominator.signum() == -1) ? denominator.negate() : denominator;
        BigInteger bunshi = (denominator.signum() == -1) ? numerator.negate() : numerator;

        bunbo = (bunshi.signum() == 0) ? BigInteger.valueOf(1L) : bunbo;
        return new Ratio(bunshi, bunbo);
    }

    public Ratio getIrreducible() {
        BigInteger gcd = numerator.gcd(denominator);
        BigInteger bunbo = denominator.divide(gcd);
        BigInteger bunshi = numerator.divide(gcd);
        return new Ratio(bunshi, bunbo).normalize();
    }
    
    @Override
    public double doubleValue() {
        
        double bunshi = numerator.doubleValue();
        double bunbo = denominator.doubleValue();
        return bunshi / bunbo;
    }
    
    
    
    @Override
    public int intValue() {

        long bunbo = denominator.longValueExact();
        if (bunbo == 1L) {
            return numerator.intValueExact();
            
        } else {
            throw new UnsupportedOperationException("Not supported yet.");
        }
    }

    @Override
    public long longValue() {

        long bunbo = denominator.longValueExact();
        if (bunbo == 1L) {
            return numerator.longValue();
            
        } else {
            throw new UnsupportedOperationException("Not supported yet.");
        }
    }

    @Override
    public float floatValue() {
        
        float bunshi = numerator.floatValue();
        float bunbo = denominator.floatValue();
        return bunshi / bunbo;
    }


    @Override
    public void prin1(OutputPort outport, PrintOption opt) {

        String num = getNumerator().toString();
        num.codePoints().forEach((i) -> write(outport, i));

        write(outport, '/');

        String de = this.getDenominator().toString();
        de.codePoints().forEach((i) -> write(outport, i));

    }
    
    private void write(OutputPort out, int codePoint ) {
        try {
            out.write(codePoint);
            
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }
        
    }
    
    

    /**
     * @return the numerator
     */
    public BigInteger getNumerator() {
        return numerator;
    }

    /**
     * @return the denominator
     */
    public BigInteger getDenominator() {
        return denominator;
    }

    
    
    public static Ratio parseRatio(String s) {

        BigDecimal bi = new BigDecimal(s);

        int s2 = bi.scale();

        BigInteger v2 = bi.unscaledValue();

        BigInteger deno = null;
        switch (s2) {
            case 1:
                deno = new BigInteger("10");
                break;
            case 2:
                deno = new BigInteger("100");
                break;
            case 3:
                deno = new BigInteger("1000");
                break;
            case 4:
                deno = new BigInteger("10000");
                break;
            case 5:
                deno = new BigInteger("100000");
                break;
            case 6:
                deno = new BigInteger("1000000");
                break;
            default:
                throw new IllegalStateException("exact");
        }

        Ratio ra = new Ratio(v2, deno);
        return ra;
    }
    
    
    
    
    /**
     * Please note, the format of the returned string may change in the future.
     * 
     * @return 
     */
    @Override
    public String toString() {
        return "Ratio{" + "numerator=" + numerator + ", denominator=" + denominator + '}';
    }
}
